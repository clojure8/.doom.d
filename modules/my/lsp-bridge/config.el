;;; my/lsp-bridge/config.el -*- lexical-binding: t; -*-

;; lsp-bridge 的后端是一个用 Python 跑的 EPC server（lsp_bridge.py），它依赖
;; `epc' 和 `orjson' 两个 Python 包。lsp-bridge 默认用 `python3' 启动后端，会随
;; PATH 解析到第一个 python——本机 PATH 里 mise / homebrew 的 python 排在 pyenv
;; 之前，而 epc/orjson 只装在 pyenv 3.12.2 里，于是后端可能静默启动失败、补全不
;; 工作（daemon 下还会卡死）。
;;
;; 这里把 `lsp-bridge-python-command' pin 到「第一个能 import epc+orjson」的解释器，
;; 优先级：环境变量 > pyenv > homebrew > 裸 python3。启动时探测一次，确定可用。
(defun +lsp-bridge--python-ok-p (py)
  "PY 可执行且能 import epc+orjson。"
  (and py (not (string-empty-p py)) (file-executable-p py)
       (zerop (call-process py nil nil nil "-c" "import epc, orjson"))))

(defun +lsp-bridge--find-python ()
  "返回第一个能 import epc+orjson 的 python 可执行路径，找不到则回退 \"python3\"。
候选用 thunk 惰性求值：`pyenv which python3' 这类 shell-out 只在前面的候选
（环境变量、pyenv 3.12.2）都没命中时才执行——否则每次启动白跑约 100ms。"
  (or (cl-some
       (lambda (thunk)
         (let ((py (funcall thunk)))
           (and (+lsp-bridge--python-ok-p py) py)))
       (list (lambda () (getenv "LSP_BRIDGE_PYTHON"))
             (lambda () (expand-file-name "~/.pyenv/versions/3.12.2/bin/python3"))
             (lambda () (ignore-errors
                          (string-trim (shell-command-to-string "pyenv which python3 2>/dev/null"))))
             (lambda () "/opt/homebrew/bin/python3")
             (lambda () "python3")))
      "python3"))

(use-package! lsp-bridge
  :init
  (setq lsp-bridge-python-command (+lsp-bridge--find-python))
  :config
  (global-lsp-bridge-mode))

;; ── acm 补全菜单：Emacs 31 起原生支持 TTY child frame，GUI/TTY 统一用 acm ──────
;; acm 的补全菜单/文档/签名都用 child-frame。`acm-frame-can-display-p' 原本用
;; `(not (display-graphic-p))' 把 TTY 一律挡掉（Emacs ≤30 的 TTY 没有 child frame）。
;; Emacs 31 已原生支持 TTY child frame，这里放开门禁，让终端里也直接用 acm 的
;; child-frame 菜单——GUI / TTY 同一套渲染，不再需要 acm-terminal/popon。
(defun +acm-frame-can-display-tty-a ()
  "放开 TTY：只要不是 noninteractive / basic-display 就允许 child-frame。
Emacs 31 原生支持 TTY child frame，故不再要求 `display-graphic-p'。"
  (not (or noninteractive emacs-basic-display)))

(after! acm-frame
  (when (>= emacs-major-version 31)
    (advice-add 'acm-frame-can-display-p :override #'+acm-frame-can-display-tty-a)))

;; ── acm 补全弹窗在 GUI 下「背景透明、后面代码透出来叠在候选上」的修复 ──────────
;; 现象：补全菜单里混进 buffer 文字（candidate 后面透出代码），看着花/错位。
;; 根因：config.el 为了主 frame 首帧就最大化+透明标题栏，把 fullscreen /
;; ns-transparent-titlebar / ns-appearance 放进了 `default-frame-alist'——而它会
;; 套到**所有**新建 frame，包括 acm 的补全 child-frame。Emacs 31 下 child frame
;; 继承 ns-transparent-titlebar 后背景变透明，于是 buffer 透出来（Emacs 30 不会）。
;; 修法：acm 建 child-frame 时把这些「主 frame 装饰参数」从 default-frame-alist 里
;; 临时剥掉，并强制不透明背景。
(defun +acm-frame-no-decoration-a (orig &rest args)
  "建 acm child-frame 时不继承主 frame 的全屏/透明/外观参数，避免补全弹窗透明。"
  ;; 必须用 let（先重绑 default-frame-alist），再在其作用域内调用 orig 建 frame。
  (let ((default-frame-alist
         (cl-remove-if (lambda (p)
                         (memq (car-safe p)
                               '(fullscreen ns-transparent-titlebar ns-appearance
                                 alpha alpha-background)))
                       default-frame-alist)))
    (let ((frame (apply orig args)))
      (when (framep frame)
        (set-frame-parameter frame 'ns-transparent-titlebar nil)
        (set-frame-parameter frame 'alpha-background 100)
        (set-frame-parameter frame 'alpha nil))
      frame)))

(after! acm-frame
  (advice-add 'acm-frame-make-frame :around #'+acm-frame-no-decoration-a))

;; ── acm-terminal：暂时停用（改用 Emacs 31 原生 TTY child frame）──────────────
;; 之前为 Emacs ≤30 的 TTY 引入 acm-terminal（popon 渲染，全局 :override advice）。
;; 升级到 Emacs 31、有了原生 TTY child frame 后不再需要，且它的全局 advice 容易
;; 干扰 GUI。保留 packages.el 里的 package! 声明，需要回退时取消下面注释即可。
;; (use-package! acm-terminal
;;   :after acm
;;   :config ...)
