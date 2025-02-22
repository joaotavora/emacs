;;; url-file.el --- File retrieval code  -*- lexical-binding:t -*-

;; Copyright (C) 1996-1999, 2004-2025 Free Software Foundation, Inc.

;; Keywords: comm, data, processes

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'mailcap)
(require 'url-vars)
(require 'url-parse)
(declare-function mm-disable-multibyte "mm-util" ())

(defvar url-allow-non-local-files nil
  "If non-nil, allow URL to fetch non-local files.
By default, this is not allowed, since that would allow rendering
HTML to fetch files on other systems if given a <img
src=\"/ssh:host...\"> element, which can be disturbing.")

(defconst url-file-default-port 21 "Default FTP port.")
(defconst url-file-asynchronous-p t "FTP transfers are asynchronous.")
(defalias 'url-file-expand-file-name 'url-default-expander)

(defun url-file-find-possibly-compressed-file (fname &rest _)
  "Find the exact file referenced by `fname'.
This tries the common compression extensions, because things like
ange-ftp is not quite smart enough to realize when a server can
do automatic decompression for them, and won't find `foo' if
`foo.gz' exists, even though the FTP server would happily serve
it up to them."
  (let ((scratch nil)
	(compressed-extensions '("" ".gz" ".z" ".Z" ".bz2" ".xz"))
	(found nil))
    (while (and compressed-extensions (not found))
      (if (file-exists-p (setq scratch (concat fname (pop compressed-extensions))))
	  (setq found scratch)))
    found))

(defun url-file-host-is-local-p (host)
  "Return t if HOST references our local machine."
  (let ((case-fold-search t))
    (or
     (null host)
     (string= "" host)
     (equal (downcase host) (downcase (system-name)))
     (and (string-match "^localhost$" host) t)
     (and (not (string-match (regexp-quote ".") host))
	  (equal (downcase host) (if (string-match (regexp-quote ".")
						   (system-name))
				     (substring (system-name) 0
						(match-beginning 0))
				   (system-name)))))))

(defun url-file-asynch-callback (_x _y name buff func args &optional efs)
  (if (not (featurep 'ange-ftp))
      ;; EFS passes us an extra argument
      (setq name buff
	    buff func
	    func args
	    args efs))
  (with-current-buffer buff
    (goto-char (point-max))
    (insert-file-contents-literally name)
    (insert (format "Content-length: %d\n\n" (buffer-size)))
    (if (not (url-file-host-is-local-p (url-host url-current-object)))
	(condition-case ()
	    (delete-file name)
	  (error nil)))
    (apply func args)))

(declare-function ange-ftp-set-passwd "ange-ftp" (host user passwd))
(declare-function ange-ftp-copy-file-internal "ange-ftp"
		  (filename newname ok-if-already-exists
			    keep-date &optional msg cont nowait))

(defun url-file-build-filename (url)
  (if (not (url-p url))
      (setq url (url-generic-parse-url url)))
  (let* ((user (url-user url))
	 (pass (url-password url))
	 (port (url-port url))
	 (host (url-host url))
	 (site (if (and port (/= port 21))
		   (if (featurep 'ange-ftp)
		       (format "%s %d" host port)
		     ;; This works in Emacs 21's ange-ftp too.
		     (format "%s#%d" host port))
		 host))
	 (file (url-unhex-string (url-filename url)))
	 (filename (cond
		    ;; ftp: URL.
		    ((or user (not (url-file-host-is-local-p host)))
		     (concat "/" (or user "anonymous") "@" site ":" file))
		    ;; file: URL on Windows.
		    ((and (string-match "\\`/[a-zA-Z]:/" file)
			  (memq system-type '(ms-dos windows-nt)))
		     (substring file 1))
		    ;; file: URL with a file:/bar:/foo-like spec.
		    ((and (not url-allow-non-local-files)
                          (string-match "\\`/[^/]+:/" file))
		     (concat "/:" file))
		    (t
		     file))))

    (and user pass
	 (cond
	  ((featurep 'ange-ftp)
	   (ange-ftp-set-passwd host user pass))
	  (t
	   nil)))

    ;; This makes sure that directories have a trailing directory
    ;; separator on them so URL expansion works right.
    ;;
    ;; FIXME?  What happens if the remote system doesn't use our local
    ;; directory-sep-char as its separator?  Would it be safer to just
    ;; use '/' unconditionally and rely on the FTP server to
    ;; straighten it out for us?
    ;; (if (and (file-directory-p filename)
    ;;          (not (string-match (format "%c$" directory-sep-char) filename)))
    ;;     (setf (url-filename url)
    ;;           (format "%s%c" filename directory-sep-char)))
    (if (and (file-directory-p filename)
	     (not (string-match "/\\'" filename)))
	(setf (url-filename url) (format "%s/" filename)))

    filename))

;;;###autoload
(defun url-file (url callback cbargs)
  "Handle file: and ftp: URLs."
  (let* ((buffer nil)
	 (uncompressed-filename nil)
	 (content-type nil)
	 (content-encoding nil)
	 (filename (url-file-build-filename url)))
    (or filename (error "File does not exist: %s" (url-recreate-url url)))
    ;; Need to figure out the content-type from the real extension,
    ;; not the compressed one.
    ;; FIXME should this regexp not include more extensions; basically
    ;; everything that url-file-find-possibly-compressed-file does?
    (setq uncompressed-filename (if (string-match "\\.\\(gz\\|Z\\|z\\)\\'" filename)
				    (substring filename 0 (match-beginning 0))
				  filename))
    (setq content-type (mailcap-extension-to-mime
			(url-file-extension uncompressed-filename))
	  content-encoding (pcase (url-file-extension filename)
			     ((or ".z" ".gz") "gzip")
			     (".Z" "compress")
			     (".uue" "x-uuencoded")
			     (".hqx" "x-hqx")
			     (".bz2" "x-bzip2")
			     (".xz" "x-xz")
			     (_ nil)))

    (if (file-directory-p filename)
	;; A directory is done the same whether we are local or remote
	(find-file filename)
      (with-current-buffer
	  (setq buffer (generate-new-buffer " *url-file*"))
        (require 'mm-util)
	(mm-disable-multibyte)
	(setq url-current-object url)
	(insert "Content-type: " (or content-type "application/octet-stream") "\n")
	(if content-encoding
	    (insert "Content-transfer-encoding: " content-encoding "\n"))
	(if (url-file-host-is-local-p (url-host url))
	      ;; Local files are handled slightly oddly
	    (if (featurep 'ange-ftp)
		(url-file-asynch-callback nil nil
					  filename
					  (current-buffer)
					  callback cbargs)
	      (url-file-asynch-callback nil nil nil
					filename
					(current-buffer)
					callback cbargs))
	  ;; FTP handling
	  (let ((new (make-temp-file
		      (format "url-tmp.%d" (user-real-uid)))))
	    (if (featurep 'ange-ftp)
		(ange-ftp-copy-file-internal filename (expand-file-name new) t
					     nil t
					     (list #'url-file-asynch-callback
						   new (current-buffer)
						   callback cbargs)
					     t))))))
    buffer))

(defmacro url-file-create-wrapper (method args)
  `(defalias ',(intern (format "url-ftp-%s" method))
     (defun ,(intern (format "url-file-%s" method)) ,args
       ,(format "FTP/FILE URL wrapper around `%s' call." method)
       (setq url (url-file-build-filename url))
       (and url (,method ,@(remove '&rest (remove '&optional args)))))))

(url-file-create-wrapper file-exists-p (url))
(url-file-create-wrapper file-attributes (url &optional id-format))
(url-file-create-wrapper file-symlink-p (url))
(url-file-create-wrapper file-readable-p (url))
(url-file-create-wrapper file-writable-p (url))
(url-file-create-wrapper file-executable-p (url))
(url-file-create-wrapper directory-files (url &optional full match nosort))
(url-file-create-wrapper file-truename (url &optional counter prev-dirs))

(provide 'url-file)

;;; url-file.el ends here
