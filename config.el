;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; fix mode line rendering artifacts
(setq ns-use-srgb-colorspace nil)


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
(setq frame-title-format "")

;; 设置 ace-window 超大字体
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "red"
    :weight bold
    :height 400))


(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; 修改默认快捷键
(map! :leader
      :desc "Execute command" "SPC" #'execute-extended-command)
(map! "s-p" #'switch-to-buffer)


;; 放大 window-select 的提示字体
(after! window-select
  (set-face-attribute 'doom-window-select-face nil :height 3.0)
  (set-face-attribute 'doom-window-select-number-face nil :height 3))


(after! centaur-tabs
  (map! :leader
        :prefix "t"
        "l" #'centaur-tabs-forward
        "h" #'centaur-tabs-backwar
        "k" #'centaur-tabs--kill-this-buffer-dont-ask)
  (evil-define-key 'normal 'global
    (kbd "t h") #'centaur-tabs-backward
    (kbd "t l") #'centaur-tabs-forward
    (kbd "t k") #'centaur-tabs--kill-this-buffer-dont-ask))


(use-package! dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (defun dwim-shell-commands-macos-open-with ()
    "Convert all marked images to jpg(s)."
    (interactive)
    (let* ((apps (seq-sort
                  #'string-lessp
                  (seq-mapcat (lambda (paths)
                                (directory-files-recursively
                                 paths "\\.app$" t (lambda (path)
                                                     (not (string-suffix-p ".app" path)))))
                              '("/Applications" "~/Applications" "/System/Applications"))))
           (selection (progn
                        (cl-assert apps nil "No apps found")
                        (completing-read "Open with: "
                                         (mapcar (lambda (path)
                                                   (propertize (file-name-base path) 'path path))
                                                 apps)))))
      (dwim-shell-command-on-marked-files
       "Open with"
       (format "open -a '%s' '<<*>>'" (get-text-property 0 'path selection))
       :silent-success t
       :no-progress t
       :utils "open"))))


