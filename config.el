;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; emacsclient -nw 偶尔出现 menu-bar 的兜底修复。
;; Doom 在 macOS 上用 'tty 哨兵延迟初始化 menu-bar，after-make-frame-functions
;; 存在竞态，server-after-make-frame-hook 在 frame 完全就绪后执行，更可靠。
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (unless (display-graphic-p)
              (set-frame-parameter nil 'menu-bar-lines 0))))

;; fix mode line rendering artifacts
(setq gc-cons-threshold (* 20 1024 1024))
(setq gc-cons-percentage 0.1)
;; 渲染优化
(setq redisplay-skip-fontification-on-input t)
(setq auto-window-vscroll nil)


;; 文件大小限制优化
(setq large-file-warning-threshold (* 500 1000 1000))  ; 500MB

;; 全局缩进：tab = 4 空格
;; tab-width 只影响真实 \t 字符的显示宽度；按 TAB 键的实际缩进量由各 mode 自己的
;; *-offset / *-indent-level 变量决定，所以这里一并设默认值。
(setq-default tab-width 4
              indent-tabs-mode nil
              evil-shift-width 4
              standard-indent 4
              ;; C / C++ / Java / Objective-C / Awk 等 cc-mode 家族
              c-basic-offset 4
              ;; Shell
              sh-basic-offset 4
              sh-indentation 4
              ;; JavaScript / TypeScript / JSON
              js-indent-level 4
              js-switch-indent-offset 4
              js2-basic-offset 4
              typescript-indent-level 4
              json-reformat:indent-width 4
              ;; Python
              python-indent-offset 4
              go-ts-mode-indent-offset 4
              ;; CSS / SCSS / Less
              css-indent-offset 4
              ;; web-mode
              web-mode-markup-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-code-indent-offset 4
              web-mode-attr-indent-offset 4
              ;; Lua / Rust / Ruby
              lua-indent-level 4
              rust-indent-offset 4
              ruby-indent-level 4
              ;; XML / SGML / HTML
              nxml-child-indent 4
              nxml-attribute-indent 4
              sgml-basic-offset 4
              ;; tree-sitter *-ts-mode 各自独立的 offset
              c-ts-mode-indent-offset 4
              c-ts-common-indent-offset 4
              typescript-ts-mode-indent-offset 4
              tsx-ts-mode-indent-offset 4
              rust-ts-mode-indent-offset 4
              ruby-ts-mode-indent-offset 4
              lua-ts-mode-indent-offset 4
              toml-ts-mode-indent-offset 4
              json-ts-mode-indent-offset 4
              yaml-ts-mode-indent-offset 4
              dockerfile-ts-mode-indent-offset 4
              cmake-ts-mode-indent-offset 4
              heex-ts-mode-indent-offset 4
              elixir-ts-mode-indent-offset 4
              nix-ts-mode-indent-offset 4
              php-ts-mode-indent-offset 4
              ;; go-ts-mode-indent-offset 在下方 go-ts-mode-hook 里另设
              ;; （Go 用真实 tab，indent-tabs-mode t）。
              ;; bash-ts-mode / js-ts-mode / python-ts-mode 复用上面的
              ;; sh-basic-offset / js-indent-level / python-indent-offset
              )


;; 快速滚动
(setq fast-but-imprecise-scrolling t)
;; 减少滚动跳动
(setq scroll-conservatively 101)
;; 鼠标滚轮优化
(setq mouse-wheel-scroll-amount '(1 ((control) . 5)))

(when (display-graphic-p)
  ;; 避免颜色失真（macOS Cocoa 专用）
  (setq ns-use-srgb-colorspace nil)

  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
  ;; (setq doom-font (font-spec :family "霞鹜文楷等宽" :size 14))
  (setq frame-title-format "")

  ;; 设置 ace-window 超大字体
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "red"
      :weight bold
      :height 400))

  ;; 像素级别平滑滚动
  (pixel-scroll-precision-mode 1)

  (add-to-list 'default-frame-alist '(fullscreen . maximized)))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-xcode)

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


;; 放大 window-select 的提示字体（GUI 下使用像素倍数，TUI 下使用固定绝对高度）
(after! window-select
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'doom-window-select-face nil :height 3.0)
        (set-face-attribute 'doom-window-select-number-face nil :height 3))
    (set-face-attribute 'doom-window-select-face nil :height 200)
    (set-face-attribute 'doom-window-select-number-face nil :height 200)))

(after! centaur-tabs
  (map! :leader
        :prefix "t"
        "l" #'centaur-tabs-forward
        "h" #'centaur-tabs-backward
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

;; 范围高亮
(setq show-paren-style 'expression
      show-paren-delay 0
      show-paren-when-point-inside-paren t)
