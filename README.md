# Nushell Tree-Sitter mode

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

A mode for Nushell that uses tree-sitter. (treesit introduced in Emacs 29)

* https://www.nushell.sh/
* https://github.com/nushell/tree-sitter-nu
* https://tree-sitter.github.io/tree-sitter/

Warning: I just threw this together and haven't used it much yet. (2023-08-05)  Not yet polished.

![example highlighting](images/sample.png)


## Features

* Syntax highlighting
* Indentation
* Completion 
  * Using completion-at-point-functions
  * Using known keywords
  * Simple variable completion using tree-sitter variable query
* Org babel integration
  * Highlighting
  * Execute source code blocks


## Installation

### Build tree-sitter-nu

1. Run `treesit-install-language-grammar`
2. Select "nu"
3. Use URL https://github.com/nushell/tree-sitter-nu
4. Pick "src"


### Install package

You will need to clone the repo and load it manually or use whatever package manager you use.

```emacs-lisp
(straight-use-package
 '(nushell-ts-mode :type git :host github :repo "herbertjones/nushell-ts-mode"))
```

For instance, on my system I have:
```emacs-lisp
(use-package nushell-ts-mode
  :straight (nushell-ts-mode :type git :host github :repo "herbertjones/nushell-ts-mode")
  :config
  (require 'nushell-ts-babel)
  (defun hfj/nushell/mode-hook ()
    (corfu-mode 1)
    (highlight-parentheses-mode 1)
    (electric-pair-local-mode 1)
    (electric-indent-local-mode 1))
  (add-hook 'nushell-ts-mode-hook 'hfj/nushell/mode-hook))

(with-eval-after-load 'org-contrib
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       ;; ...
       (nushell . t))))
```
