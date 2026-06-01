;; -*- no-byte-compile: t; -*-
;;; my/web/packages.el

;; 本模块不引入新包：
;;   - web-mode / css-mode / scss-mode / emmet-mode 由 Doom `:lang web` 提供
;;   - js/ts/jsx/tsx 的 major-mode 由 Doom `:lang javascript` 与内置 *-ts-mode 提供
;;   - 补全后端 lsp-bridge 由 `:my lsp-bridge` 提供
;;
;; 真正的语言能力来自外部 language server（npm 全局安装）：
;;   npm i -g vscode-langservers-extracted   # html / css LS
;;   npm i -g typescript typescript-language-server
;;   npm i -g @vue/language-server            # volar
;;   npm i -g vscode-eslint-language-server   # 或随 vscode-langservers-extracted
;;   npm i -g emmet-ls
