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

(defun bc-path (index-alist pos)
  "Get breadcrumb for position POS given INDEX-ALIST."
  (cl-labels ((dfs (node &optional path)
                (or (and (consp (cdr node))
                         (cl-loop with path = (cons (car node) path)
                                  for child in (cdr node)
                                  thereis (dfs child path)))
                    (and (let ((reg (get-text-property 0 'breadcrumb-region
                                                       (car node))))
                           (cond (reg (<= (car reg) pos (cdr reg))) (t)))
                         (cons (car node) path)))))
    (nreverse
     (cond ((get-text-property 0 'breadcrumb-region (caar index-alist))
            (cl-loop for a in index-alist thereis (dfs a)))
           (t
            (cl-loop for (a b) on index-alist
                     if (consp (cdr a)) collect a into non-terminal
                     else do
                     (when (< (cdr a) pos (or (and b (cdr b)) (point-max)))
                       (cl-return (list (car a))))))))))

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

(provide 'breadcrumb)
;;; breadcrumb.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("bc-" . "breadcrumb-"))
;; End:
