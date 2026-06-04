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

;; ── acm-terminal：TTY 下的 acm 补全；GUI 仍用原生 child-frame ────────────────
;; acm 的补全菜单/文档用 child-frame，Emacs 30 的 TTY 不支持 child frame，于是
;; 终端里补全弹窗出不来。acm-terminal 用 popon（overlay 弹窗）顶替——但它是
;; **全局** `:override' advice（acm-terminal-active/-deactive 成对加/删 advice），
;; all-or-nothing；且 require 时 `(unless window-system (acm-terminal-active))'
;; 在 daemon（启动 window-system=nil）会全局激活，把 GUI frame 也变成 popon。
;;
;; 需求：GUI frame 用原生 acm，TTY frame 用 acm-terminal。daemon 下两类 frame 共存，
;; 所以按「当前选中 frame 是否图形界面」动态、幂等地 toggle。
(use-package! acm-terminal
  :after acm
  :config
  (defvar +acm-terminal--active 'unset
    "记录 acm-terminal advice 当前激活态，避免无谓地反复加/删 advice。")
  (defun +acm-terminal-sync (&rest _)
    "按选中 frame：GUI → 关 acm-terminal（原生 child-frame）；TTY → 开（popon）。幂等。"
    (let ((want (not (display-graphic-p))))
      (unless (eq want +acm-terminal--active)
        (if want (acm-terminal-active) (acm-terminal-deactive))
        (setq +acm-terminal--active want))))
  ;; 抵消加载时 `(unless window-system (acm-terminal-active))' 的默认激活，
  ;; 复位后按当前 frame 校正一次。
  (acm-terminal-deactive)
  (setq +acm-terminal--active nil)
  (+acm-terminal-sync)
  ;; 新建 frame / 切换焦点时重新判定（emacsclient -nw 与 GUI 窗口可并存）。
  (add-hook 'server-after-make-frame-hook #'+acm-terminal-sync)
  (add-hook 'window-selection-change-functions #'+acm-terminal-sync))
