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
