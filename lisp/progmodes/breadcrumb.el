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

(defun bc--path-1 (index-alist pos) )

(defvar-local bc--path-2-cache nil)
(defun bc--path-2 (index-alist pos)
  (cl-labels ((dfs (n &optional path)
                (setq path (cons (car n) path))
                (if (consp (cdr n))
                    (mapc (lambda (n) (dfs n path)) (cdr n))
                  (push (cons (cdr n) path) bc--path-2-cache))))
    (unless bc--path-2-cache
      (mapc #'dfs index-alist)
      (setq bc--path-2-cache (cl-sort bc--path-2-cache #'< :key #'car)))
    (cl-loop with l = bc--path-2-cache
             with min = 0
             with max = (length l)
             for i = (+ min (/ (- max min) 2))
             for x = (elt l i)
             if (< pos (car x)) do (setq max i)
             else do (setq min i)
             if (= min (1- max)) return (cdr (elt l min)))))

(benchmark-run 1000 (bc--path-2 (imenu--make-index-alist) (point)))



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

(provide 'breadcrumb)
;;; breadcrumb.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("bc-" . "breadcrumb-"))
;; End:
