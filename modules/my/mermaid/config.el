;;; my/mermaid/config.el -*- lexical-binding: t; -*-

;; Mermaid diagram support:
;; - .mmd / .mermaid major mode
;; - C-c C-c / localleader commands compile diagrams through mmdc
;; - Org Babel `#+begin_src mermaid :file out.svg' execution

(defun +mermaid/mmdc-command ()
  "Return the Mermaid CLI executable path."
  (or (executable-find "mmdc")
      (let ((mise-mmdc (expand-file-name "~/.local/share/mise/shims/mmdc"))
            (npm-mmdc (expand-file-name "~/.npm-global/bin/mmdc"))
            (bun-mmdc (expand-file-name "~/.bun/bin/mmdc")))
        (cond
         ((file-executable-p mise-mmdc) mise-mmdc)
         ((file-executable-p npm-mmdc) npm-mmdc)
         ((file-executable-p bun-mmdc) bun-mmdc)
         (t "mmdc")))))

(defun +mermaid/setup-cli ()
  "Configure mermaid-mode to use the installed Mermaid CLI."
  (setq mermaid-mmdc-location (+mermaid/mmdc-command)
        mermaid-output-format ".svg"
        mermaid-flags "-b transparent"))

(use-package! mermaid-mode
  :mode (("\\.mmd\\'" . mermaid-mode)
         ("\\.mermaid\\'" . mermaid-mode))
  :commands (mermaid-mode
             mermaid-compile
             mermaid-compile-file
             mermaid-compile-buffer
             mermaid-compile-region
             mermaid-open-browser
             mermaid-open-doc
             org-babel-execute:mermaid)
  :init
  (+mermaid/setup-cli)
  :config
  (+mermaid/setup-cli)
  (setq mermaid-indentation-level 2)
  (map! :map mermaid-mode-map
        :localleader
        "c" #'mermaid-compile
        "f" #'mermaid-compile-file
        "b" #'mermaid-compile-buffer
        "r" #'mermaid-compile-region
        "o" #'mermaid-open-browser
        "d" #'mermaid-open-doc))

(after! org
  ;; mermaid-mode also provides `org-babel-execute:mermaid', but Org needs the
  ;; package loaded before enabling the babel language.
  (require 'mermaid-mode)
  (+mermaid/setup-cli)
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid))
  (add-to-list 'org-babel-default-header-args:mermaid '(:results . "file"))
  (add-to-list 'org-babel-default-header-args:mermaid '(:exports . "results"))
  (add-to-list 'org-babel-default-header-args:mermaid '(:background-color . "transparent"))
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
