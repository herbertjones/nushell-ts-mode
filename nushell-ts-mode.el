;;; nushell-ts-mode.el --- Tree-sitter support for Nushell  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Herbert Jones

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
;; A mode for Nushell that uses tree-sitter.  (treesit introduced in Emacs 29)

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

(defvar nushell-ts-mode--prettify-symbols-alist
  '((">=" . ?≥)
    ("<=" . ?≤)
    ("!=" . ?≠)
    ("->" . ?→)
    ("=>" . ?⇒)
    ("inf" . ?∞))
  "Alist of symbol prettifications used for `prettify-symbols-alist'.")

(defun nushell-ts-mode--get-variable-names ()
  "Get a list of variable names that are near point."
  (interactive)
  ;; Ensuring the treesitter is ready and a root node exists, else throw an
  ;; error
  (unless (and (treesit-ready-p 'nu) (treesit-buffer-root-node))
    (error "No treesitter node"))

  (let ((node (treesit-node-at (point)))
        (variable-names (list))
        function-node)
    ;; If there is no node at the current point, throw an error
    (unless node
      (error "No treesitter node"))

    ;; Looping through the tree from the current node upwards to find a function
    ;; node
    (cl-loop for examine-node = node then (treesit-node-parent examine-node)
             while examine-node
             do (let ((node-type (treesit-node-type examine-node)))
                  (cond ((equal node-type "decl_def")
                         (setq function-node examine-node)))))

    ;; When a function node is found, query and capture all variable names
    ;; within its scope
    (when function-node
      (dolist (pair (treesit-query-capture function-node '((stmt_let (identifier) @name))))
        (push (concat "$" (treesit-node-text (cdr pair) t))
              variable-names)))

    ;; Return the list of variable names
    variable-names))

;;;###autoload
(defun nushell-ts-completions-at-point ()
  "Completion function for Nushell.
Provides completion suggestions at the current point."
  (let* ((bnds (bounds-of-thing-at-point 'symbol))
         (start (car bnds))
         (end (cdr bnds)))
    ;; Return a list containing:
    ;; - the start and end positions of the symbol at the point
    ;; - a merged list of completion suggestions, including operators, keywords, types,
    ;;   and nearby variable names (if any are retrieved without errors)
    ;; - an :exclusive 'no' keyword argument indicating that other completion sources
    ;;   can contribute suggestions as well
    (list start end
          (append nushell-ts-mode--operators nushell-ts-mode--keywords nushell-ts-mode--types
                  (ignore-errors (nushell-ts-mode--get-variable-names)))
          :exclusive 'no)))

(defvar nushell-ts-mode--indent-rules
  `((nu
     ;; If the current node is any closing bracket, the indentation should be at
     ;; the beginning of the line
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)

     ;; For nodes within a block, the indentation should be at the beginning of
     ;; the line plus the offset value defined in
     ;; 'nushell-ts-mode-indent-offset'
     ((parent-is "block") parent-bol nushell-ts-mode-indent-offset)

     ;; Similar to blocks, for nodes within a string, the indentation should be
     ;; at the beginning of the line plus the offset value
     ((parent-is "string") parent-bol nushell-ts-mode-indent-offset)

     ;; Define indentation rules for array elements
     ((parent-is "array") parent-bol nushell-ts-mode-indent-offset)

     ;; Define indentation rules for list values
     ((parent-is "val_list") parent-bol nushell-ts-mode-indent-offset)

     ;; Define indentation rules for expressions enclosed in parentheses
     ((parent-is "expr_parenthesized") parent-bol nushell-ts-mode-indent-offset)

     ;; Define indentation rules for parameters enclosed in brackets
     ((parent-is "parameter_bracks") parent-bol nushell-ts-mode-indent-offset)

     ;; If there is no node, the indentation should be at the beginning of the
     ;; line
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

(defvar nushell-ts-mode--syntax-table
  (let ((synTable (make-syntax-table)))
    ;; The '#' character starts a comment
    (modify-syntax-entry ?# "<" synTable)
    ;; The newline character ('\n') ends a comment
    (modify-syntax-entry ?\n ">#" synTable)
    synTable)
  "Syntax table for `nushell-ts-mode'.")

;;;###autoload
(define-derived-mode nushell-ts-mode prog-mode "NuShell"
  "Major mode for editing NuShell scripts."
  :group 'nushell
  :syntax-table nushell-ts-mode--syntax-table
  ;; Ensure the treesitter parser for 'nu' is ready before setting up the major
  ;; mode
  (when (treesit-ready-p 'nu)
    (treesit-parser-create 'nu)

    ;; Setting up comment syntax, '#' is used to start a comment and it doesn't
    ;; require an end symbol
    (setq-local comment-start "#")
    (setq-local comment-end "")

    ;; Indent.
    ;; Configure the indentation rules using a predefined set of rules stored in
    ;; 'nushell-ts-mode--indent-rules'
    (setq-local treesit-simple-indent-rules nushell-ts-mode--indent-rules)

    ;; A regexp that matches the node type of defun nodes.  For example,
    ;; "(function|class)_definition".
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("decl_def"
                              "stmt_let"
                              "assignment")))
    (setq-local treesit-defun-name-function #'nushell-ts-mode--defun-name)

    ;; Setting up font-lock (syntax highlighting) settings using predefined
    ;; settings and feature list
    (setq-local treesit-font-lock-settings nushell-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( error keyword string types)
                  ( function number operator property variable)
                  ( bracket )))

    ;; Setup for Imenu, defining what tree-sitter nodes should appear in the
    ;; Imenu index
    (setq-local treesit-simple-imenu-settings
                `(("Variable" "\\`stmt_let\\'" nil nil)
                  ("Variable" "\\`assignment\\'" nil nil)
                  ("Function" "\\`decl_def\\'" nil nil)))

    ;; Prettify symbols settings, to replace certain symbols with more visually
    ;; appealing ones
    (setq-local prettify-symbols-alist nushell-ts-mode--prettify-symbols-alist)

    ;; Setting up electric indent characters and rules for automatic indentation
    (setq-local electric-indent-chars
                (append "{}()[]" electric-indent-chars))
    (setq-local electric-layout-rules
                '((?\; . after) (?\{ . after) (?\} . before)))

    ;; Adding a hook to enable completion at point functionality in the major
    ;; mode
    (add-hook 'completion-at-point-functions #'nushell-ts-completions-at-point nil t)

    ;; Final setup for the major mode using tree-sitter
    (treesit-major-mode-setup)))

(when (treesit-ready-p 'nu)
  (add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-ts-mode))
  (add-to-list 'interpreter-mode-alist '("nu" . nushell-ts-mode)))

(provide 'nushell-ts-mode)

;;; nushell-ts-mode.el ends here
