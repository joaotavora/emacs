;;; refactor.el --- Code refactoring interface       -*- lexical-binding: t; -*-

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
(require 'diff)

(defgroup refactor nil
  "Refactoring commands."
  :prefix "refactor-"
  :group 'applications)

(defun refactor--region-bounds () ; utilities
  "Region bounds if active, else bounds of things at point."
  (if (use-region-p) `(,(region-beginning) ,(region-end))
    (let ((boftap (bounds-of-thing-at-point 'sexp)))
      (list (car boftap) (cdr boftap)))))

(defun refactor--read-execute-code-action (_actions))



;;;; Backend-contacting functions
(defun refactor--advertised-action-kinds ()
  '("quickfix" "refactor.extract" "refactor.inline"
    "refactor.rewrite" "source.organizeImports"))

(defun refactor--actions-1 (_beg _end _kind))



(defun refactor-actions (beg &optional end kind interactive)
  (interactive
   `(,@(refactor--region-bounds)
     ,(and current-prefix-arg
           (completing-read "[refactor] Action kind: "
                            (refactor--advertised-action-kinds)))
     t))
  (let ((actions (refactor--actions-1 beg end kind)))
    (if interactive
        (refactor--read-execute-code-action actions)
      actions)))

(defcustom refactor-confirm-server-edits '((refactor-rename . nil)
                                           (t . maybe-summary))
  "Control if changes proposed by LSP should be confirmed with user.

If this variable's value is the symbol `diff', a diff buffer is
pops up, allowing the user to apply each change individually.  If
the symbol `summary' or any other non-nil value, the user is
prompted in the minibuffer with aa short summary of changes.  The
symbols `maybe-diff' and `maybe-summary' mean that the
confirmation is offered to the user only if the changes target
files visited in buffers.  Finally, a nil value means all changes
are applied directly without any confirmation.

If this variable's value can also be an alist ((COMMAND . ACTION)
...) where COMMAND is a symbol designating a command, such as
`refactor-rename', `refactor-code-actions',
`refactor-code-action-quickfix', etc.  ACTION is one of the symbols
described above.  The value `t' for COMMAND is accepted and its
ACTION is the default value for commands not in the alist."
  :type (let ((basic-choices
               '((const :tag "Use diff" diff)
                 (const :tag "Summarize and prompt" summary)
                 (const :tag "Maybe use diff" maybe-diff)
                 (const :tag "Maybe summarize and prompt" maybe-summary)
                 (const :tag "Don't confirm" nil))))
          `(choice ,@basic-choices
                   (alist :tag "Per-command alist"
                          :key-type (choice (function :tag "Command")
                                            (const :tag "Default" t))
                          :value-type (choice . ,basic-choices)))))

(cl-defstruct refactor--edit beg end text)

(cl-defun refactor--apply-text-edits (edits &optional silent)
  "Apply EDITS for current buffer.
EDITS are objects created with `refactor-make-edit'.  If SILENT,
don't echo progress in mode-line."
  (unless edits (cl-return-from refactor--apply-text-edits))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (unless silent
                       (make-progress-reporter
                        (format "[refactor] applying %s edits to `%s'..."
                                howmany (current-buffer))
                        0 howmany)))
           (done 0))
      (mapc (lambda (edit)
              (pcase-let ((source (current-buffer))
                          ((cl-struct refactor--edit beg end text) edit))
                (with-temp-buffer
                  (insert text)
                  (let ((temp (current-buffer)))
                    (with-current-buffer source
                      (save-excursion
                        (save-restriction
                          (narrow-to-region beg end)
                          (replace-buffer-contents temp)))
                      (when reporter
                        (progress-reporter-update reporter (cl-incf done))))))))
            (reverse edits))
      (undo-amalgamate-change-group change-group)
      (when reporter
        (progress-reporter-done reporter)))))

(defun refactor--confirm-server-edits (origin _prepared)
  "Helper for `refactor--apply-workspace-edit.
ORIGIN is a symbol designating a command.  Reads the
`refactor-confirm-server-edits' user option and returns a symbol
like `diff', `summary' or nil."
  (let (v)
    (cond ((symbolp refactor-confirm-server-edits) refactor-confirm-server-edits)
          ((setq v (assoc origin refactor-confirm-server-edits)) (cdr v))
          ((setq v (assoc t refactor-confirm-server-edits)) (cdr v)))))

(defun refactor--propose-changes-as-diff (prepared)
  "Helper for `refactor--apply-workspace-edit'.
Goal is to popup a `diff-mode' buffer containing all the changes
of PREPARED, ready to apply with C-c C-a.  PREPARED is a
list ((FILENAME EDITS VERSION)...)."
  (with-current-buffer (get-buffer-create "*EGLOT proposed server changes*")
    (buffer-disable-undo (current-buffer))
    (let ((inhibit-read-only t)
          (target (current-buffer)))
      (diff-mode)
      (erase-buffer)
      (pcase-dolist (`(,path ,edits ,_) prepared)
        (with-temp-buffer
          (let* ((diff (current-buffer))
                 (existing-buf (find-buffer-visiting path))
                 (existing-buf-label (prin1-to-string existing-buf)))
            (with-temp-buffer
              (if existing-buf
                  (insert-buffer-substring existing-buf)
                (insert-file-contents path))
              (refactor--apply-text-edits edits t)
              (diff-no-select (or existing-buf path) (current-buffer) nil t diff)
              (when existing-buf
                ;; Here we have to pretend the label of the unsaved
                ;; buffer is the actual file, just so that we can
                ;; diff-apply without troubles.  If there's a better
                ;; way, it probably involves changes to `diff.el'.
                (with-current-buffer diff
                  (goto-char (point-min))
                  (while (search-forward existing-buf-label nil t)
                    (replace-match (buffer-file-name existing-buf))))))
            (with-current-buffer target
              (insert-buffer-substring diff))))))
    (setq-local buffer-read-only t)
    (buffer-enable-undo (current-buffer))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (font-lock-ensure)))

(defun refactor--apply-workspace-edit (wedit origin)
  "Apply (or offer to apply) the workspace edit WEDIT.
ORIGIN is a symbol designating the command that originated this
edit proposed by the server."
  (refactor--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared
           (mapcar (refactor--lambda ((TextDocumentEdit) textDocument edits)
                     (refactor--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (refactor-uri-to-path uri) edits version)))
                   documentChanges)))
      (unless (and changes documentChanges)
        ;; We don't want double edits, and some servers send both
        ;; changes and documentChanges.  This unless ensures that we
        ;; prefer documentChanges over changes.
        (cl-loop for (uri edits) on changes by #'cddr
                 do (push (list (refactor-uri-to-path uri) edits) prepared)))
      (cl-flet ((notevery-visited-p ()
                  (cl-notevery #'find-buffer-visiting
                               (mapcar #'car prepared)))
                (accept-p ()
                  (y-or-n-p
                   (format "[eglot] Server wants to edit:\n%sProceed? "
                           (cl-loop
                            for (f eds _) in prepared
                            concat (format
                                    "  %s (%d change%s)\n"
                                    f (length eds)
                                    (if (> (length eds) 1) "s" ""))))))
                (apply ()
                  (cl-loop for edit in prepared
                   for (path edits version) = edit
                   do (with-current-buffer (find-file-noselect path)
                        (refactor--apply-text-edits edits version))
                   finally (eldoc) (refactor--message "Edit successful!"))))
        (let ((decision (refactor--confirm-server-edits origin prepared)))
          (cond
           ((or (eq decision 'diff)
                (and (eq decision 'maybe-diff) (notevery-visited-p)))
            (refactor--propose-changes-as-diff prepared))
           ((or (memq decision '(t summary))
                (and (eq decision 'maybe-summary) (notevery-visited-p)))
            (when (accept-p) (apply)))
           (t
            (apply))))))))

(defun refactor-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          nil nil nil nil
          (symbol-name (symbol-at-point)))))
  (refactor-server-capable-or-lose :renameProvider)
  (refactor--apply-workspace-edit
   (refactor--request (refactor--current-server-or-lose)
                   :textDocument/rename `(,@(refactor--TextDocumentPositionParams)
                                          :newName ,newname))
   this-command))

(provide 'refactor)
;;; refactor.el ends here
