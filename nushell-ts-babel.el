;;; nushell-ts-babel.el --- org babel support for Nushell  -*- lexical-binding: t; -*-

(require 'ob-core)

;; Assuming your custom mode is defined in a file called 'my-mode.el'
(require 'nushell-ts-mode)

(defcustom nushell-ts-babel-nu-path "nu"
  "Path to the Nushell \"nu\" command."
  :type 'string
  :group 'nushell-ts-babel)

;; Define a function to execute code for your custom language
(defun org-babel-execute:nushell (body params)
  "Execute a block of Nushell commands with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((tmp-src-file (org-babel-temp-file "nushell-src-" ".nu")))
    (with-temp-file tmp-src-file
      (insert body))
    (let ((results (org-babel-eval (concat nushell-ts-babel-nu-path " --no-config-file " tmp-src-file) "")))
      (when results
        (setq results (org-trim (org-remove-indentation results)))
        (org-babel-reassemble-table
         (org-babel-result-cond (cdr (assq :result-params params))
           (org-babel-read results t)
           (let ((tmp-file (org-babel-temp-file "nushell-")))
             (with-temp-file tmp-file (insert results))
             (org-babel-import-elisp-from-file tmp-file)))
         (org-babel-pick-name
          (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
         (org-babel-pick-name
          (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

;; Connect the language to your custom major mode for syntax highlighting
(add-to-list 'org-src-lang-modes '("nushell" . nushell-ts))

(provide 'nushell-ts-babel)
