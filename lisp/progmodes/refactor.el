;;; refactor.el --- Backend-agnostic refactoring interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Version: 0.1
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "28.1"))

;; This is a GNU ELPA :core package.  Avoid adding functionality that
;; is not available in the version of Emacs recorded above.

;; This file is part of GNU Emacs.

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

;; This library provides a common interface for tools that offer to
;; rewrite a program: extracting a function, inlining a variable,
;; renaming an identifier, organizing imports, or fixing a diagnostic.
;;
;; It is deliberately ignorant of how those rewrites are discovered.
;; A "backend" is any object returned by `refactor-backend-functions';
;; methods of the `refactor-backend-*' generic functions dispatch on
;; it, in the manner of `xref-backend-functions'.  Unlike Xref, every
;; applicable backend contributes.
;;
;; The two halves of this library are independent and either is useful
;; alone:
;;
;; - Discovery and selection: `refactor' asks the backends what they
;;   can do here and lets the user pick.
;;
;; - Application: `refactor-apply-changeset' takes a description of
;;   changes to a project -- edits to files, and the creation, renaming
;;   and deletion of files -- and carries them out, having first shown
;;   them to the user as a summary or a diff, according to
;;   `refactor-confirmation'.  A backend may call this directly, and
;;   should, for changes it was not asked for.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'flymake)


;;;; Backends

(defvar refactor-backend-functions nil
  "Special hook to find the refactor backends for the current context.
Each function on this hook is called in turn with no arguments, and
should return either nil to mean that it is not applicable, or a
refactor backend, a value to dispatch the `refactor-backend-*'
generic functions.  Unlike `xref-backend-functions', from which
this takes its shape, every applicable backend contributes: the
actions offered by all backends are merged into a single list for
the user to choose from.

FIXME: This hook is probably overkill: a plain buffer-local
variable of backends would likely do.  But a hook lets a backend
decide lazily whether it applies, and matches the Xref precedent.")

(defun refactor-find-backends ()
  "Return the refactor backends applicable in the current context.
Run every function on `refactor-backend-functions' in turn and
collect the non-nil backends they return."
  (let (retval)
    (run-hook-wrapped 'refactor-backend-functions
                      (lambda (a)
                        (when-let* ((x (funcall a))) (push x retval))))
    retval))

(cl-defgeneric refactor-backend-name (backend)
  "Return a short human-readable name for BACKEND."
  (:method (backend) (symbol-name backend)))

(cl-defgeneric refactor-backend-bounds (backend)
  "Return (BEG END) BACKEND should consider for actions at point.
BEG and END delimit the program expression, diagnostic or region
that a refactoring here would act on."
  (:method (_backend)
   (let (diags boftap)
     (cond ((use-region-p) `(,(region-beginning) ,(region-end)))
           ((setq diags (flymake-diagnostics (point) (point)))
            (cl-loop for d in diags
                     minimizing (flymake-diagnostic-beg d) into beg
                     maximizing (flymake-diagnostic-end d) into end
                     finally (cl-return (list beg end))))
           ((setq boftap (bounds-of-thing-at-point 'sexp))
            (list (car boftap) (cdr boftap)))
           (t
            (list (point) (point)))))))

(cl-defgeneric refactor-backend-actions
    (backend beg end &key kind callback trigger-kind)
  "Return refactoring actions BACKEND offers between BEG and END.

KIND, if non-nil, restricts the result to that kind and its
sub-kinds.

If CALLBACK is nil, return the list of `refactor-action' objects
directly; blocking to do so is acceptable.

If CALLBACK is non-nil, either return the list directly anyway,
when that is cheap, or return `:async' and arrange for CALLBACK to
be called with the list eventually.  CALLBACK may be called from
any buffer; staleness is handled by the caller.")

(cl-defgeneric refactor-backend-rename-default (backend)
  "Return the new name to offer for the identifier at point, or nil.
A nil return means BACKEND does not claim the identifier."
  (:method (_backend) nil))

(cl-defgeneric refactor-backend-rename (backend newname)
  "Return a changeset renaming the identifier at point to NEWNAME."
  (:method (_backend _newname) nil))

(defvar-local refactor--serial 0
  "Serial number of the most recent action discovery round.")

(cl-defun refactor--merge (actions new-actions)
  "Merge NEW-ACTIONS into ACTIONS, returning the new list.
When two actions share the same title, the one already in ACTIONS
wins, so among backends the one earliest in
`refactor-backend-functions' has priority."
  (dolist (a new-actions)
    (unless (cl-some (lambda (other)
                       (equal (refactor-action-title other)
                              (refactor-action-title a)))
                     actions)
      (push a actions)))
  (nreverse actions))

(cl-defun refactor--collect
    (beg end &key kind callback trigger-kind
          &aux (serial (cl-incf refactor--serial))
               (slots (mapcar (lambda (backend) (list backend nil nil nil))
                              (refactor-find-backends)))
               (actions '())
               collecting)
  "Gather actions for BEG END from every applicable backend.

If CALLBACK is nil, return the merged list of `refactor-action's.

If CALLBACK is non-nil, merge results into CALLBACK as backends
finish, and return the list gathered so far, or `:async' if some
backend has not finished yet."
  (cl-labels
      ((deliver (slot)
         (when (and (not collecting)
                    (not (nth 1 slot))
                    (listp (nth 2 slot)))
           (setf (nth 1 slot) t)
           (let ((result (nth 2 slot)))
             (unless (eq result :async)
               (setq actions (refactor--merge actions result))
               (when callback (funcall callback actions)))))))
    (setq collecting t)
    (dolist (slot slots)
      (condition-case-unless-debug oops
          (let ((result
                 (refactor-backend-actions
                  (car slot) beg end
                  :kind kind
                  :trigger-kind trigger-kind
                  :callback (lambda (result)
                              (when (= serial refactor--serial)
                                (setf (nth 3 slot) t
                                      (nth 2 slot) result)
                                (deliver slot))))))
            ;; A backend may call CALLBACK before returning.  When it
            ;; does, trust the callback's result over the return value.
            (unless (nth 3 slot)
              (setf (nth 2 slot) result)))
        (error
         (message "refactor: backend %S failed: %S"
                  (car slot) (cdr oops)))))
    (setq collecting nil)
    (mapc #'deliver slots))
  (if callback
      (if (cl-some (lambda (slot)
                     (and (null (nth 1 slot)) (eq (nth 2 slot) :async)))
                   slots)
          :async
        actions)
    actions))


;;;; Commands
;;;
(defvar refactor-kinds)

(defun refactor-bounds ()
  "Return (BEG END) for the current refactoring context.
The active region, if any, else the union of Flymake diagnostics at
point, else the bounds of the expression at point, else point."
  (let (diags boftap)
    (cond ((use-region-p) `(,(region-beginning) ,(region-end)))
          ((setq diags (flymake-diagnostics (point) (point)))
           (cl-loop for d in diags
                    minimizing (flymake-diagnostic-beg d) into beg
                    maximizing (flymake-diagnostic-end d) into end
                    finally (cl-return (list beg end))))
          ((setq boftap (bounds-of-thing-at-point 'sexp))
           (list (car boftap) (cdr boftap)))
          (t
           (list (point) (point))))))

(cl-defgeneric refactor-backend-execute (backend action)
  "Have BACKEND carry ACTION out, by whatever means it likes."
  (:method (_backend _action)
   (error "Refactor backend doesn't know how to execute actions")))

(cl-defun refactor--execute-action (action)
  "Carry ACTION out."
  (refactor-backend-execute (refactor-action-backend action) action))

(defun refactor--read-execute-action (actions interactive)
  "Choose and execute one of ACTIONS, a list of `refactor-action's.
Interactively, if there is only one, execute it without asking.
If INTERACTIVE is nil, just return ACTIONS."
  (let* ((menu-items (cl-loop for a in actions
                              collect (cons (refactor-action-title a) a)))
         (preferred-action
          (cl-find-if (lambda (menu-item)
                        (refactor-action-preferred (cdr menu-item)))
                      menu-items))
         (default-action (car (or preferred-action (car menu-items))))
         (chosen (if (and interactive (null (cdr menu-items)))
                     (cdr (car menu-items))
                   (if (listp last-nonmenu-event)
                       (x-popup-menu last-nonmenu-event
                                     `("Refactor:"
                                       ("dummy" ,@menu-items)))
                     (cdr (assoc (completing-read
                                  (format "Pick a refactoring (default %s): "
                                          default-action)
                                  menu-items nil t nil nil default-action)
                                 menu-items))))))
    (if interactive
        (when chosen (refactor--execute-action chosen))
      actions)))

(cl-defun refactor (beg &optional end kind interactive)
  "Find refactoring actions between BEG and END, and offer to run them.

If KIND is non-nil, only consider actions of that kind and its
sub-kinds; the kinds themselves are from `refactor-kinds'.

Interactively, BEG and END default to `refactor-bounds', and a
prefix argument prompts for KIND.  When INTERACTIVE is nil, return
the list of `refactor-action' objects."
  (interactive
   `(,@(refactor-bounds)
     ,(and current-prefix-arg
           (intern
            (completing-read
             "Kind of refactoring: "
             (mapcar (lambda (kind)
                       (cons (symbol-name kind) kind))
                     refactor-kinds)
             nil t)))
     t))
  (let ((actions (refactor--collect beg end :kind kind)))
    (unless actions
      (user-error (if kind "No \"%s\" refactorings here" "No refactorings here")
                  kind))
    (refactor--read-execute-action actions interactive)))

(defmacro refactor--define-kind-command (name kind)
  "Define NAME to execute KIND refactorings between BEG and END."
  `(defun ,name (beg &optional end)
     ,(format "Execute `%s' refactorings between BEG and END." kind)
     (interactive (refactor-bounds))
     (refactor beg end ',kind t)))

(defun refactor--mouse-call (what &optional update-mode-line)
  "Make an interactive lambda for calling WHAT with the mouse."
  (lambda (event)
    (interactive "e")
    (let ((start (event-start event)))
      (with-selected-window (posn-window start)
        (save-excursion
          (unless (posn-area start)
            (goto-char (posn-point start)))
          (call-interactively what)
          (when update-mode-line
            (force-mode-line-update t)))))))

(defalias 'refactor-at-mouse (refactor--mouse-call 'refactor)
  "Like `refactor', but intended for mouse events.")

(refactor--define-kind-command refactor-organize-imports organize-imports)
(refactor--define-kind-command refactor-extract extract)
(refactor--define-kind-command refactor-inline inline)
(refactor--define-kind-command refactor-rewrite rewrite)
(refactor--define-kind-command refactor-quickfix quickfix)

(cl-defun refactor-rename (newname backend)
  "Rename the symbol at point to NEWNAME via BACKEND.
Interactively, BACKEND is chosen to be the first backend in
`refactor-backend-functions' that claims the symbol at point."
  (interactive
   (pcase-let ((`(,backend . ,sym-name)
                (cl-loop for b in (refactor-find-backends)
                         when (refactor-backend-rename-default b)
                         return (cons b it))))
     (unless backend
       (user-error "No backend can rename the symbol at point"))
     (list
      (read-from-minibuffer (format "Rename `%s' to: " sym-name)
       nil nil nil nil sym-name)
      backend)))
  (refactor-apply-changeset (refactor-backend-rename backend newname)
                            :origin this-command))


;;;; Kinds
;;;
;; Kinds are symbols in a hierarchy, so that asking for `refactor'
;; also offers extractions, and so that a backend can register a kind
;; nobody anticipated under whichever known kind it most resembles.

(defvar refactor-kinds nil
  "List of all refactoring kinds defined so far, in definition order.")

(defmacro refactor-define-kind (name parent docstring)
  "Define NAME as a kind of refactoring.
PARENT is the kind NAME specializes, or nil if NAME is itself a
top-level kind.  DOCSTRING describes NAME to the user."
  (declare (doc-string 3) (indent 2))
  `(progn
     (put ',name 'refactor-kind-parent ',parent)
     (put ',name 'refactor-kind-documentation ,docstring)
     (add-to-list 'refactor-kinds ',name)
     (setq refactor-kinds (nreverse refactor-kinds))
     ',name))

(defun refactor-kind-documentation (kind)
  "Return the docstring describing KIND, or nil."
  (get kind 'refactor-kind-documentation))

(defun refactor-kind-matches-p (kind filter)
  "Return non-nil if KIND is FILTER or one of its sub-kinds.
A nil FILTER matches everything."
  (or (null filter)
      (cl-loop for k = kind then (get k 'refactor-kind-parent)
               while k thereis (eq k filter))))

(refactor-define-kind quickfix nil
  "Fix a problem reported at point.")

(refactor-define-kind refactor nil
  "Change code without changing what it does.")

(refactor-define-kind extract refactor
  "Extract code into a new named entity.")

(refactor-define-kind inline refactor
  "Inline a named entity into the places that use it.")

(refactor-define-kind rewrite refactor
  "Restate code in a different form.")

(refactor-define-kind move refactor
  "Move an entity somewhere else.")

(refactor-define-kind source nil
  "Act on the whole file rather than on a selection.")

(refactor-define-kind organize-imports source
  "Tidy up the file's import declarations.")

(refactor-define-kind fix-all source
  "Apply every fix available in the file.")


;;;; Actions

(defclass refactor-action ()
  ((title :initarg :title :initform nil :accessor refactor-action-title
          :documentation "One-line description, shown to the user.")
   (kind :initarg :kind :initform nil :accessor refactor-action-kind
         :documentation "A symbol from `refactor-kinds', or nil.")
   (preferred :initarg :preferred :initform nil :accessor refactor-action-preferred
              :documentation "Non-nil if this is the obvious choice here.")
   (backend :initarg :backend :initform nil :accessor refactor-action-backend
            :documentation "The backend that offered this action.")
   (data :initarg :data :initform nil :accessor refactor-action-data
         :documentation "Opaque payload, meaningful to the backend."))
  :documentation "A refactoring a backend offers to perform.")


;;;; Changesets
;;;
;; A changeset is an ordered list of operations, each an instance of a
;; class below.  Backends make them with `make-instance', and may
;; subclass the classes for operations we haven't thought of;
;; `refactor-apply-changeset' shows them to the user and carries them
;; out.

(defclass refactor-operation ()
  ((description :initarg :description :initform nil
                :accessor refactor-operation-description
                :documentation "\
Overrides the description this operation would otherwise give of
itself in prompts and summaries."))
  :documentation "Abstract superclass of the operations making up a changeset."
  :abstract t)

(defclass refactor-file-edit (refactor-operation)
  ((file :initarg :file :initform nil :accessor refactor-file-edit-file
         :documentation "Absolute name of the file to change.")
   (edits :initarg :edits :initform nil :accessor refactor-file-edit-edits
          :documentation "\
Either a list of (BEG END NEWTEXT), where BEG and END are integer
positions valid in the widened buffer visiting the file, or a
function of no arguments returning such a list, called with that
buffer current.  Edits must not overlap."))
  :documentation "An operation changing the text of a single file.")

(defclass refactor-file-creation (refactor-operation)
  ((file :initarg :file :initform nil :accessor refactor-file-creation-file
         :documentation "Absolute name of the file to create.")
   (contents :initarg :contents :initform nil
             :accessor refactor-file-creation-contents
             :documentation "Initial contents, or nil for an empty file.")
   (if-exists :initarg :if-exists :initform 'error
              :accessor refactor-file-creation-if-exists
              :documentation "\
What to do when the file already exists: `error', `skip' or `overwrite'."))
  :documentation "An operation creating a file.")

(defclass refactor-file-renaming (refactor-operation)
  ((from :initarg :from :initform nil :accessor refactor-file-renaming-from
         :documentation "Absolute name of the file to rename.")
   (to :initarg :to :initform nil :accessor refactor-file-renaming-to
       :documentation "Absolute name to rename it to.")
   (if-exists :initarg :if-exists :initform 'error
              :accessor refactor-file-renaming-if-exists
              :documentation "\
What to do when the new name is taken: `error', `skip' or `overwrite'."))
  :documentation "An operation renaming a file.")

(defclass refactor-file-deletion (refactor-operation)
  ((file :initarg :file :initform nil :accessor refactor-file-deletion-file
         :documentation "Absolute name of the file to delete.")
   (recursive :initarg :recursive :initform nil
              :accessor refactor-file-deletion-recursive
              :documentation "Non-nil to delete a directory's contents too.")
   (if-missing :initarg :if-missing :initform 'error
               :accessor refactor-file-deletion-if-missing
               :documentation "\
What to do when the file does not exist: `error' or `skip'."))
  :documentation "An operation deleting a file.")

(cl-defgeneric refactor-operation-kind (operation)
  "Return a symbol classifying OPERATION.
One of `edit', `create', `rename' or `delete'.  These are the
symbols `refactor-confirmation' matches against."
  (:method ((_ refactor-file-edit)) 'edit)
  (:method ((_ refactor-file-creation)) 'create)
  (:method ((_ refactor-file-renaming)) 'rename)
  (:method ((_ refactor-file-deletion)) 'delete))

(cl-defgeneric refactor--describe (operation)
  "Return a one-line description of OPERATION."
  (:method ((op refactor-file-edit))
   (let ((edits (refactor-file-edit-edits op)))
     (if (functionp edits)
         (format "Change `%s'" (refactor-file-edit-file op))
       (format "Change `%s' (%d change%s)" (refactor-file-edit-file op)
               (length edits) (if (cdr edits) "s" "")))))
  (:method ((op refactor-file-creation))
   (format "Create `%s'" (refactor-file-creation-file op)))
  (:method ((op refactor-file-renaming))
   (format "Rename `%s' to `%s'" (refactor-file-renaming-from op)
           (refactor-file-renaming-to op)))
  (:method ((op refactor-file-deletion))
   (format "Delete `%s'" (refactor-file-deletion-file op))))

(defun refactor-operation-summary (operation)
  "Return the description of OPERATION to show the user."
  (or (refactor-operation-description operation)
      (refactor--describe operation)))

(defun refactor-operation-file (operation)
  "Return the file OPERATION acts on, or nil.
For a renaming, this is the file as it is named now."
  (cl-typecase operation
    (refactor-file-edit (refactor-file-edit-file operation))
    (refactor-file-creation (refactor-file-creation-file operation))
    (refactor-file-renaming (refactor-file-renaming-from operation))
    (refactor-file-deletion (refactor-file-deletion-file operation))))


;;;; Applying edits to a buffer

(cl-defun refactor-apply-text-edits (edits &key silent)
  "Apply EDITS to the current buffer as a single undo step.

EDITS is a list of (BEG END NEWTEXT), or a function of no
arguments returning such a list, which is called with this buffer
current.  BEG and END are positions valid in the widened buffer.
Edits must not overlap, but need not be sorted.

Unless SILENT, report progress in the echo area."
  (when (functionp edits) (setq edits (funcall edits)))
  (unless edits (cl-return-from refactor-apply-text-edits))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (unless silent
                       (make-progress-reporter
                        (format "Applying %s edits to `%s'..."
                                howmany (current-buffer))
                        0 howmany)))
           (done 0))
      ;; Convert every edit to markers up front, so that applying one
      ;; cannot invalidate the positions of the others.
      (mapc (pcase-lambda (`(,beg ,end ,newtext))
              (if (> emacs-major-version 30)
                  (replace-region-contents beg end newtext)
                ;; Before Emacs 31, `replace-region-contents' wanted a
                ;; function, so go through a temporary buffer.
                (let ((source (current-buffer)))
                  (with-temp-buffer
                    (insert newtext)
                    (let ((temp (current-buffer)))
                      (with-current-buffer source
                        (save-excursion
                          (save-restriction
                            (narrow-to-region beg end)
                            (with-no-warnings
                              (replace-buffer-contents temp)))))))))
              (when reporter
                (progress-reporter-update reporter (cl-incf done))))
            (mapcar (pcase-lambda (`(,beg ,end ,newtext))
                      (list (copy-marker beg) (copy-marker end) newtext))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (when reporter (progress-reporter-done reporter)))))


;;;; Applying a changeset

(defcustom refactor-confirmation '((t . maybe-summary))
  "Control if changes proposed by a backend should be confirmed with user.

If this variable's value is the symbol `diff', a diff buffer pops
up, allowing the user to apply each change individually.  If the
symbol `summary' or any other non-nil value, the user is prompted
in the minibuffer with a short summary of changes.  The symbols
`maybe-diff' and `maybe-summary' mean that the confirmation is
offered to the user only if the changes target files not visited
in buffers.  Finally, a nil value means all changes are applied
directly without any confirmation.

This variable's value can also be an alist ((KEY . ACTION) ...)
where KEY is either a symbol designating the invoked Emacs command
\(such as `refactor-rename', `refactor', `refactor-quickfix',
etc.), or a list of file operation kinds (`create', `rename',
`delete') contained in the changeset.  ACTION is one of the
symbols described above.  The value t for KEY is accepted and its
ACTION is the default value for commands or file operation kinds
not in the alist."
  :type (let ((basic-choices
               '((const :tag "Use diff" diff)
                 (const :tag "Summarize and prompt" summary)
                 (const :tag "Maybe use diff" maybe-diff)
                 (const :tag "Maybe summarize and prompt" maybe-summary)
                 (const :tag "Don't confirm" nil))))
          `(choice ,@basic-choices
                   (alist :tag "Per-command or per-kind alist"
                          :key-type (choice (function :tag "Command")
                                            (repeat :tag "File operation kinds" symbol)
                                            (const :tag "Default" t))
                          :value-type (choice . ,basic-choices))))
  :group 'tools
  :version "32.1")

(defconst refactor--changes-buffer-name "*refactor changes*"
  "Name of the buffer used to preview proposed changes as a diff.")

(defvar refactor-obsolete-command-alist nil
  "Alist of (NEW-COMMAND . OLD-COMMAND), for compatibility of keys.
`refactor-confirmation' keys may name commands that have since
been renamed; this tells `refactor--confirmation' which OLD-COMMAND
a NEW-COMMAND is the successor of, so that a user's configuration
keeps matching.")

(defun refactor--confirmation (origin operations)
  "Return the confirmation ACTION to use for OPERATIONS.
ORIGIN is a symbol designating the command that asked for them.
Reads `refactor-confirmation' and returns a symbol such as `diff'
or `summary', or nil to apply without confirming."
  (let ((v (lambda (candidate) (cdr (assq candidate refactor-confirmation))))
        (obsolete (cdr (assq origin refactor-obsolete-command-alist)))
        kinds)
    (or (and (symbolp refactor-confirmation) refactor-confirmation)
        ;; Check for command-based entry, accepting an obsolete
        ;; command name for the invoking one.
        (funcall v origin)
        (and obsolete
             (let ((old (funcall v obsolete)))
               (when old
                 (lwarn 'refactor :warning
                        "Key %S in `refactor-confirmation' names an \
obsolete command; use %S instead."
                        obsolete origin)
                 old)))
        ;; Check for operation-kind-based entry
        (and (setq kinds (mapcar #'refactor-operation-kind operations))
             (cl-some (lambda (entry)
                        (and (listp (car entry))
                             (cl-some (lambda (kind)
                                        (memq kind (car entry)))
                                      kinds)
                             (cdr entry)))
                      refactor-confirmation))
        ;; Default entry
        (funcall v t))))

(cl-defgeneric refactor--apply-operation (operation)
  "Carry OPERATION out."
  (:method ((op refactor-file-edit))
   (with-current-buffer (find-file-noselect (refactor-file-edit-file op))
     (refactor-apply-text-edits (refactor-file-edit-edits op))))
  (:method ((op refactor-file-creation))
   (let* ((path (refactor-file-creation-file op))
          (if-exists (refactor-file-creation-if-exists op))
          (exists (file-exists-p path)))
     (when (and exists (eq if-exists 'error))
       (error "File %s already exists" path))
     (when (or (not exists) (eq if-exists 'overwrite))
       (let ((dir (file-name-directory path)))
         (unless (file-directory-p dir) (make-directory dir t)))
       (write-region (or (refactor-file-creation-contents op) "")
                     nil path nil 'nomessage))))
  (:method ((op refactor-file-renaming))
   (let* ((old (refactor-file-renaming-from op))
          (new (refactor-file-renaming-to op))
          (if-exists (refactor-file-renaming-if-exists op))
          (new-exists (file-exists-p new)))
     (when (and new-exists (eq if-exists 'error))
       (error "File %s already exists" new))
     (unless (and new-exists (eq if-exists 'skip))
       (let ((dir (file-name-directory new)))
         (unless (file-directory-p dir) (make-directory dir t)))
       ;; If the old file is visited, rename the buffer too
       (when-let* ((buf (find-buffer-visiting old)))
         (with-current-buffer buf (set-visited-file-name new t t)))
       (rename-file old new (eq if-exists 'overwrite)))))
  (:method ((op refactor-file-deletion))
   (let* ((path (refactor-file-deletion-file op))
          (exists (file-exists-p path)))
     (when (and (not exists) (eq (refactor-file-deletion-if-missing op) 'error))
       (error "File %s does not exist" path))
     (when exists
       ;; Kill the buffer if the file is visited
       (when-let* ((buf (find-buffer-visiting path))) (kill-buffer buf))
       (delete-file path (refactor-file-deletion-recursive op))))))

(defun refactor--apply-and-report (operation)
  "Carry OPERATION out and say so in the echo area."
  (refactor--apply-operation operation)
  (unless (refactor-file-edit-p operation)
    (message "%s" (replace-regexp-in-string
                   "^\\([^ ]+\\) " "\\1d "
                   (refactor-operation-summary operation)))))

(defun refactor--file-text (file)
  "Insert the full text of FILE into the current buffer.
If a buffer is visiting FILE, take the text from there instead,
disregarding any narrowing, so that positions in the result agree
with positions in the widened buffer."
  (if-let* ((buf (find-buffer-visiting file)))
      (insert (with-current-buffer buf
                (save-restriction
                  (widen)
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (insert-file-contents file)))

(defun refactor--propose-changes-as-diff (operations)
  "Pop up a `diff-mode' buffer with the changes OPERATIONS would make.
OPERATIONS are all `refactor-file-edit's.  The buffer is ready to
apply with \\<diff-mode-map>\\[diff-apply-buffer]."
  (with-current-buffer (get-buffer-create refactor--changes-buffer-name)
    (buffer-disable-undo (current-buffer))
    (let ((inhibit-read-only t)
          (target (current-buffer)))
      (diff-mode)
      (erase-buffer)
      (dolist (op operations)
        (with-temp-buffer
          (let* ((diff (current-buffer))
                 (path (refactor-file-edit-file op))
                 (existing-buf (find-buffer-visiting path))
                 (existing-buf-label (prin1-to-string existing-buf)))
            (with-temp-buffer
              (refactor--file-text path)
              (refactor-apply-text-edits (refactor-file-edit-edits op)
                                         :silent t)
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
    (font-lock-ensure)
    (current-buffer)))

(cl-defun refactor-apply-changeset (operations &key origin)
  "Apply OPERATIONS, a changeset, after confirming with the user.

OPERATIONS is a list of `refactor-operation' objects, applied in
order.  ORIGIN is a symbol designating the command that produced
them; `refactor-confirmation' may key on it.

Return a list (APPLIED REASON) saying whether the changes were
applied and, if not, why not."
  (cl-labels
      ((user-accepts-p ()
         (y-or-n-p
          (format "These changes will be made:\n%s\nProceed? "
                  (mapconcat (lambda (op)
                               (concat "  " (refactor-operation-summary op)))
                             operations "\n"))))
       (apply-all ()
         (mapc #'refactor--apply-and-report operations)
         (eldoc)
         (message "Refactoring applied")
         `(t nil)))
    (let* ((decision (refactor--confirmation origin operations))
           (all-edits (cl-every #'refactor-file-edit-p operations))
           (peaceful (and all-edits
                          (cl-every (lambda (op)
                                      (find-buffer-visiting
                                       (refactor-file-edit-file op)))
                                    operations))))
      (cond
       ((and (memq decision '(maybe-diff maybe-summary)) peaceful)
        (apply-all))
       ((memq decision '(diff maybe-diff))
        (cond (all-edits
               (pop-to-buffer (refactor--propose-changes-as-diff operations))
               `(nil "decision to apply manually"))
              (t
               ;; `map-y-or-n-p' heroics.  Iterate over operations with
               ;; individual prompts, showing diffs for edits.
               (let* ((wconf (current-window-configuration))
                      (remaining operations)
                      (applied 0)
                      (total (length operations)))
                 (unwind-protect
                     (progn
                       (map-y-or-n-p
                        (lambda (op)
                          (when (refactor-file-edit-p op)
                            (display-buffer
                             (refactor--propose-changes-as-diff (list op))))
                          (format "%s? " (refactor-operation-summary op)))
                        (lambda (op)
                          (set-window-configuration wconf)
                          (refactor--apply-and-report op)
                          (cl-incf applied))
                        (lambda ()
                          ;; Skip edits to files that don't exist (e.g.
                          ;; the user skipped the create operation).
                          (cl-loop for op = (pop remaining) while op
                                   when (or (not (refactor-file-edit-p op))
                                            (file-exists-p
                                             (refactor-file-edit-file op)))
                                   return op))
                        '("change" "changes" "apply"))
                       (if (= applied total)
                           (progn
                             (eldoc)
                             (message "Refactoring applied")
                             `(t nil))
                         `(nil "decision to abort")))
                   (set-window-configuration wconf))))))
       ((memq decision '(t summary maybe-summary))
        (if (user-accepts-p) (apply-all) `(nil "decision to decline")))
       ((apply-all))))))

(provide 'refactor)
;;; refactor.el ends here
