;; -*- no-byte-compile: t; -*-
;;; my/python/packages.el

(package! python-mode)
(package! pyvenv)
(package! poetry)
(package! pipenv)
(package! py-isort)
(package! py-autopep8)
(package! blacken)
(package! python-pytest)
(package! python-docstring)
(package! sphinx-doc)
(package! pydoc)
(package! python-info)
(package! live-py-mode)
(package! transient)  ; transient 菜单支持
;; Jupyter 支持
(package! ein)        ; Emacs IPython Notebook
(package! jupyter)    ; Emacs Jupyter
(package! zmq)        ; ZeroMQ 支持，Jupyter 需要
;; uv 支持通过自定义函数实现，无需额外包
(package! anaconda-mode :disable t)  ; 禁用 anaconda-mode，使用 lsp-bridge
(package! company-anaconda :disable t)  ; 禁用 company-anaconda，使用 lsp-bridge 的补全