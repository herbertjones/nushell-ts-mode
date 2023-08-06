;;; nushell-ts-mode.el --- tree-sitter support for Nushell  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Herbert Jones <jones.herbert@gmail.com>
;; Maintainer : Herbert Jones <jones.herbert@gmail.com>
;; Created    : August 2023
;; Homepage   : https://github.com/herbertjones/nushell-ts-mode
;; Keywords   : nu nushell languages tree-sitter

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Still in early stages.  I haven't used it much, but I wanted something that
;; was usable.

;;; Code:

(require 'treesit)

(declare-function treesit-font-lock-rules "treesit.c")
(declare-function treesit-major-mode-setup "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-ready-p "treesit.c")

(defcustom nushell-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `nushell-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'nushell)

(defvar nushell-ts-mode--operators
  '("+"           ; add
    "-"           ; subtract
    "*"           ; multiply
    "/"           ; divide
    "//"          ; floor division
    "mod"         ; modulo
    "**"          ; exponentiation (power)
    "=="          ; equal
    "!="          ; not equal
    "<"           ; less than
    "<="          ; less than or equal
    ">"           ; greater than
    ">="          ; greater than or equal
    "=~"          ; regex match / string contains another
    "!~"          ; inverse regex match / string does not contain another
    "in"          ; value in list
    "not-in"      ; value not in list
    "not"         ; logical not
    "and"         ; and two Boolean expressions (short-circuits)
    "or"          ; or two Boolean expressions (short-circuits)
    "xor"         ; exclusive or two boolean expressions
    "bit-or"      ; bitwise or
    "bit-xor"     ; bitwise xor
    "bit-and"     ; bitwise and
    "bit-shl"     ; bitwise shift left
    "bit-shr"     ; bitwise shift right
    "starts-with" ; string starts with
    "ends-with"   ; string ends with
    "++")         ; append lists
  "Nushell operators for tree-sitter font-locking.")

(defvar nushell-ts-mode--keywords
  '("alias"
    "def"
    "let"
    "mut"
    "source"))

(defvar nushell-ts-mode--types
  '("any"
    "block"
    "bool"
    "cell-path"
    "cond"
    "duration"
    "error"
    "expr"
    "filesize"
    "glob"
    "int"
    "list"
    "math"
    "number"
    "operator"
    "path"
    "range"
    "record"
    "signature"
    "string"
    "table"
    "variable"))

(defun nushell-ts-mode--get-variable-names ()
  "Get a list of variable names that are near point."
  (interactive)
  (unless (and (treesit-ready-p 'nu) (treesit-buffer-root-node))
    (error "No treesitter node"))
  (let ((node (treesit-node-at (point)))
        (variable-names (list))
        function-node)
    (unless node
      (error "No treesitter node"))

    (cl-loop for examine-node = node then (treesit-node-parent examine-node)
             while examine-node
             do (let ((node-type (treesit-node-type examine-node)))
                  (cond ((equal node-type "decl_def")
                         (setq function-node examine-node)))))
    (when function-node
      (dolist (pair (treesit-query-capture function-node '((stmt_let (identifier) @name))))
        (push (concat "$" (treesit-node-text (cdr pair) t))
              variable-names)))
    variable-names))

(defun nushell-ts-completions-at-point ()
  "Completion for Nushell"
  (let* ((bnds (bounds-of-thing-at-point 'symbol))
         (start (car bnds))
         (end (cdr bnds)))
    (list start end
          (append nushell-ts-mode--operators nushell-ts-mode--keywords nushell-ts-mode--types
                  (ignore-errors (nushell-ts-mode--get-variable-names)))
          :exclusive 'no)))

(defvar nushell-ts-mode--indent-rules
  `((nu
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol nushell-ts-mode-indent-offset)
     ((parent-is "string") parent-bol nushell-ts-mode-indent-offset)
     ((parent-is "array") parent-bol nushell-ts-mode-indent-offset)
     ((parent-is "val_list") parent-bol nushell-ts-mode-indent-offset)
     ((parent-is "expr_parenthesized") parent-bol nushell-ts-mode-indent-offset)
     ((parent-is "parameter_bracks") parent-bol nushell-ts-mode-indent-offset)
     (no-node parent-bol 0))))

(defun nushell-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("stmt_let"
     (treesit-node-text
      (treesit-node-child node 1) t))
    ("assignment"
     (treesit-node-text
      (treesit-node-child-by-field-name node "lhs") t))
    ("decl_def"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))))

(defvar nushell-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'nu
   :feature 'property
   '((record_entry (identifier) @font-lock-property-name-face))

   :language 'nu
   :feature 'comment
   '([(comment) (shebang)] @font-lock-comment-face)

   :language 'nu
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'nu
   :feature 'definition
   '((stmt_let (identifier) @font-lock-variable-name-face)
     (assignment (val_variable) @font-lock-variable-name-face)
     (decl_def (cmd_identifier) @font-lock-function-name-face)
     (parameter (identifier) @font-lock-variable-name-face))

   :language 'nu
   :feature 'function
   '((command (cmd_identifier) @font-lock-function-call-face)
     (decl_alias (cmd_identifier) @font-lock-function-call-face))

   :language 'nu
   :feature 'keyword
   `([,@nushell-ts-mode--keywords] @font-lock-keyword-face)

   :language 'nu
   :feature 'number
   '((val_number)
     @font-lock-number-face)

   :language 'nu
   :feature 'operator
   `([,@nushell-ts-mode--operators] @font-lock-operator-face)

   :language 'nu
   :feature 'string
   '((val_string) @font-lock-string-face)

   :language 'nu
   :feature 'types
   `([,@nushell-ts-mode--types] @font-lock-type-face)

   :language 'nu
   :feature 'variable
   '((val_variable) @font-lock-variable-use-face)

   :language 'nu
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Font-lock settings for Nushell.")

(define-derived-mode nushell-ts-mode prog-mode "NuShell"
  "Major mode for editing NuShell scripts."
  :group 'nushell
  ;; :syntax-table nushell-ts-mode--syntax-table
  (when (treesit-ready-p 'nu)
    (treesit-parser-create 'nu)

    ;; Comments
    (setq-local comment-start "#")
    (setq-local comment-end "")

    ;; Indent.
    (setq-local treesit-simple-indent-rules nushell-ts-mode--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("decl_def"
                              "stmt_let"
                              "assignment")))
    (setq-local treesit-defun-name-function #'nushell-ts-mode--defun-name)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings nushell-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( error keyword string types)
                  ( function number operator property variable)
                  ( bracket )))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Variable" "\\`stmt_let\\'" nil nil)
                  ("Variable" "\\`assignment\\'" nil nil)
                  ("Function" "\\`decl_def\\'" nil nil)))

    (setq-local electric-indent-chars
                (append "{}()[]" electric-indent-chars))
    (setq-local electric-layout-rules
                '((?\; . after) (?\{ . after) (?\} . before)))

    (add-hook 'completion-at-point-functions 'nushell-ts-completions-at-point nil t)

    (treesit-major-mode-setup)))

(when (treesit-ready-p 'nu)
  (add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-ts-mode))
  (add-to-list 'interpreter-mode-alist '("nu" . nushell-ts-mode)))

(provide 'nushell-ts-mode)

;;; nushell-ts-mode.el ends here
