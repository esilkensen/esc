;;; es-mode.el --- an Extensible Syntax editing mode

;; Author:  Erik Silkensen (silkense@colorado.edu)
;; Version: 29 November 2011

;;; Commentary:

;; Installation:
;;
;;  - put `es-mode.el' somewhere in your emacs load path
;;  - add these lines to your .emacs file:
;;    (autoload 'es-mode "es-mode" nil t)
;;    (add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

;;; Code:

(defvar es-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `es-mode'.")

(defconst es-font-lock-keywords
  (let ((module "module[ ]+\\([a-zA-Z_][a-zA-Z_0-9]*\\)")
	(builtins (regexp-opt '("declare" "forall" "import") t))
	(constants (regexp-opt '("::=") t))
	(keywords (regexp-opt '("module" "types") t))
	(identifier "|[^a-zA-Z_0-9]+\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"))
    (list
     (list module 1 font-lock-function-name-face)
     (cons "\\<Type\\>" font-lock-type-face)
     (cons (concat "\\<" builtins "\\>") font-lock-builtin-face)
     (cons (concat "\\<" constants "\\>") font-lock-constant-face)
     (cons (concat "\\<" keywords "\\>") font-lock-keyword-face)))
  "Keyword highlighting specification for `es-mode'.")

;;;###autoload
(define-derived-mode es-mode fundamental-mode "Extensible Syntax"
  "Major mode for editing Extensible Syntax files."
  :syntax-table es-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(es-font-lock-keywords)))

(provide 'es-mode)
;;; es-mode.el ends here
