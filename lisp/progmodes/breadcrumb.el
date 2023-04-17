;;; breadcrumb.el --- imenu-based breadcrumb paths   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'imenu)

(cl-defun bc-bisect (a x &key (from 0) (to (length a)) key from-end)
  "Compute index to insert X in sequence A, keeping it sorted.
If X already in A, the resulting index is the leftmost such
index, unless FROM-END is t.  KEY is as usual in other CL land."
  (cl-macrolet ((search (from-end key)
                  `(cl-loop while (< from to)
                            for mid = (/ (+ from to) 2)
                            for p1 = (elt a mid)
                            for p2 = ,(if key `(funcall key p1) `p1)
                            if (,(if from-end '< '<=) x p2)
                            do (setq to mid) else do (setq from (1+ mid))
                            finally return from)))
    (if from-end (if key (search t key) (search t nil))
      (if key (search nil key) (search nil nil)))))

(defvar-local bc--path-1-cache nil)
(defun bc--path-1 (index-alist pos)
  (cl-labels
      ((search (nodes &optional path)
         (cl-loop
          for n in nodes
          for reg = (get-text-property 0 'breadcrumb-region (car n))
          when (<= (car reg) pos (cdr reg))
          return (search (cdr n) (cons (car n) path))
          finally (cl-return path))))
    (nreverse (search index-alist))))

(defvar-local bc--path-2-cache nil)
(defun bc--path-2 (index-alist pos)
  (cl-labels ((dfs (n &optional path)
                (setq path (cons (car n) path))
                (if (consp (cdr n))
                    (mapc (lambda (n) (dfs n path)) (cdr n))
                  (setq bc--path-2-cache
                        (vconcat bc--path-2-cache
                                 `[,(cons (cdr n) path)])))))
    (unless bc--path-2-cache
      (mapc #'dfs index-alist)
      (setq bc--path-2-cache (cl-sort bc--path-2-cache #'< :key #'car)))
    (unless (< pos (car (aref bc--path-2-cache 0)))
      (let ((res (bc-bisect bc--path-2-cache pos :key #'car :from-end t)))
        (unless (zerop res) (reverse (cdr (elt bc--path-2-cache (1- res)))))))))

(defun bc-path (index-alist pos)
  "Get breadcrumb for position POS given INDEX-ALIST."
  (if (get-text-property 0 'breadcrumb-region (caar index-alist))
      (bc--path-1 index-alist pos)
    (bc--path-2 index-alist pos)))

(defvar bc--last-update-tick 0)

(defvar bc--header-line-key [header-line mouse-1])

(defun bc--format-node (p)
  (let ((reg (get-text-property 0 'breadcrumb-region p)))
    (if reg
        (propertize p
                    'mouse-face 'header-line-highlight
                    'help-echo "Go here"
                    'keymap (let ((m (make-sparse-keymap)))
                              (define-key m bc--header-line-key
                                          (lambda (&rest _e)
                                            (interactive)
                                            (push-mark)
                                            (goto-char (car reg))))
                              m))
      p)))

(defun bc-path-for-header-line ()
  (cl-loop with alist =
           (if (and imenu--index-alist
                    (= (buffer-chars-modified-tick) bc--last-update-tick))
               imenu--index-alist
             (setq bc--last-update-tick (buffer-chars-modified-tick))
             (imenu--make-index-alist))
           for (p . more) on (bc-path alist (point))
           collect (bc--format-node p) when more collect " > "))

(defvar bc-header-line-format
  '(:eval (bc-path-for-header-line)))

(define-minor-mode bc-mode
  "Header lines with breadcrumbs."
  :init-value nil
  (if bc-mode (add-to-list 'header-line-format bc-header-line-format)
    (setq header-line-format (delq bc-header-line-format header-line-format))))

(defun bc-jump ()
  (interactive)
  (let (cands choice)
    (cl-labels
        ((fmt (strs)
           (mapconcat #'identity strs " > "))
         (dfs (nodes &optional path)
           (cl-loop
            for n in nodes
            for pos = (or (car (get-text-property 0 'breadcrumb-region (car n)))
                          (and (number-or-marker-p (cdr n)) (cdr n)))
            when pos do (push (cons (fmt (reverse path)) pos)
                              cands)
            do (dfs (cdr n) path))))
      (imenu--make-index-alist)
      (dfs imenu--index-alist)
      (unless cands (user-error "Sorry, no breadcrumb items to jump to."))
      (setq choice (cdr (assoc (completing-read "Index item? " cands nil t)
                               cands #'string=)))
      (push-mark)
      (goto-char choice))))

(provide 'breadcrumb)
;;; breadcrumb.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("bc-" . "breadcrumb-") ("s-" . "breadcrumb-segment-tree-"))
;; End:
