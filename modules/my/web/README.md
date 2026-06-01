# my/web — 前端 Web 开发模块

为 HTML / CSS / JS / Vue / React 等前端文件提供 **lsp-bridge** 补全、诊断与 emmet 缩写展开。

补全后端统一走 [`:my lsp-bridge`](../lsp-bridge)，本模块只负责把各类前端文件
正确映射到对应的 language server，并补齐 lsp-bridge 默认配置里缺失的部分
（HTML 的 emmet、`.jsx` 的 React 语义、`.scss/.less` 的归属、相关 major-mode 的
自动启用钩子）。

## 支持的文件类型

| 文件               | major-mode             | language server (lsp-bridge)            |
| ------------------ | ---------------------- | --------------------------------------- |
| `.html` `.htm`     | `web-mode`             | `html_emmet`（html LS + emmet-ls）      |
| `.css`             | `css-mode`/`css-ts`    | `vscode-css-language-server`            |
| `.scss` `.sass`    | `scss-mode`/`sass`     | `vscode-css-language-server`            |
| `.less`            | `less-css-mode`        | `vscode-css-language-server`            |
| `.js` `.mjs` `.cjs`| `js-mode`/`js-ts`      | `javascript`                            |
| `.jsx`             | `tsx-ts-mode`          | `javascriptreact`（React）              |
| `.ts` `.mts`       | `typescript-ts-mode`   | `typescript_eslint`（TS + eslint）      |
| `.tsx`             | `tsx-ts-mode`          | `typescriptreact_eslint`（React + TS）  |
| `.vue`             | `web-mode`             | `volar_vtsls_emmet`（volar+vtsls+emmet）|

> lsp-bridge 的服务器选择是「扩展名优先、其次 major-mode」，因此即便多种文件都用
> `web-mode` 打开（如 `.html` 和 `.vue`），也能按扩展名精确区分到不同 server。

## Vue 为什么需要特殊处理

**volar 3.x 只负责 `.vue` 的模板/样式**；脚本（TS/JS）区域的补全、跳转、诊断
已全部移交给「带 `@vue/typescript-plugin` 的 tsserver」。lsp-bridge 自带的
`.vue → volar_emmet` 里**只有 volar、没有 tsserver**，所以脚本区毫无补全——这正是
“CSS/HTML 正常、Vue 不正常”的根因。此外 lsp-bridge 自带的 `vtsls.json` 把插件
路径写死成 `/opt/homebrew/...`，与本机安装位置可能不符。

本模块的 `+web/setup-vue-lsp` 在启动时：

1. 据 `executable-find` 解析出 `vue-language-server` / `tsc` 的**真实路径**，
   推导出 `@vue/typescript-plugin` 的位置与 `tsdk`（随 node 安装位置/版本自适应）；
2. 动态生成正确的 `vtsls.json`、`volar.json` 和组合 multiserver
   `volar_vtsls_emmet.json`（volar 管模板 + vtsls 管脚本 + emmet 管缩写），
   写入 `<doom-cache>/lsp-bridge-web/` 并通过
   `lsp-bridge-user-langserver-dir` / `lsp-bridge-user-multiserver-dir` 让
   lsp-bridge 优先采用；
3. 把 `.vue` 指向该组合 multiserver。

缺 `vtsls` 或 `vue-language-server` 时静默跳过，保留 lsp-bridge 内置
`volar_emmet` 作兜底。升级 lsp-bridge 不会丢失（配置在缓存目录，不动包文件）。

## 依赖的 language server

需在 `PATH` 中可用（全局 npm 安装）：

```bash
npm i -g vscode-langservers-extracted   # vscode-html / vscode-css / vscode-eslint LS
npm i -g typescript typescript-language-server
npm i -g @vue/language-server            # volar（模板/样式）
npm i -g @vtsls/language-server          # vtsls（Vue 脚本区 TS 智能，必装）
npm i -g emmet-ls                        # emmet 缩写补全
```

> Vue 的脚本补全依赖 `vtsls` + `@vue/typescript-plugin`（插件随 `@vue/language-server`
> 一起安装）。请把这些 server 装在 **Emacs `exec-path` 能找到的同一套 node** 里
> （与 `vue-language-server` 同目录），否则插件路径推导会落到另一套 node。

缺少 `emmet-ls` 时，HTML 改用纯 `vscode-html-language-server`，Vue 的组合
multiserver 自动去掉 emmet（仅 volar + vtsls）。缺 `vtsls` 时 `.vue` 回退到
lsp-bridge 内置 `volar_emmet`（仅模板，无脚本补全）。

## 缩进：web = 2 空格，存在 `.editorconfig` 时以它为准

- 各 web 文件类型的缩进偏移在 `$DOOMDIR/config.el` 的 `setq-default` 里统一设为
  **2**（`web-mode-*-indent-offset`、`css-indent-offset`、`js-indent-level`、
  `typescript-indent-level`、`typescript-ts-mode-indent-offset`、
  `tsx-ts-mode-indent-offset`、`json-ts-mode-indent-offset`、`sgml-basic-offset`
  等；非 web 语言保持 4）。
- 本模块额外用 `set-indent-vars!` 登记了 `web-mode` / `tsx-ts-mode` /
  `json-ts-mode`（Doom 的自动猜测对这三者会失败），使其 `tab-width` 与
  `evil-shift-width`（`>>` 缩进量）也同步为 2。
- **存在 `.editorconfig` 时一切以它为准**：Doom 的 editorconfig 模块在
  `prog-mode-hook` 末尾应用，晚于设默认值的 `doom-set-indent`，会把缩进偏移、
  `tab-width`、`evil-shift-width` 全部覆盖为 `.editorconfig` 指定的值。

> 已实测：无 `.editorconfig` 的 `.html` → 2 空格（`tab-width`/`>>`/offset 均为 2）；
> 目录内放 `indent_size = 4` 的 `.editorconfig` 后同一文件 → 全部变 4。

## 快捷键（localleader，前缀 `SPC m w`）

在 `web-mode` / js / ts / css 等前端 buffer 中可用：

| 键          | 命令                       | 说明                              |
| ----------- | -------------------------- | --------------------------------- |
| `SPC m w o` | `+web/browse-current-file` | 用默认浏览器打开当前 HTML 文件    |
| `SPC m w r` | `+web/npm-run`             | 选择并运行 `package.json` 的脚本  |
| `SPC m w i` | `+web/npm-install`         | 在最近的 `package.json` 处装依赖  |
| `SPC m w f` | `+web/prettier-format`     | `npx prettier --write` 格式化当前文件 |

补全/缩写展开本身由 lsp-bridge 自动提供：输入即弹出候选；emmet 缩写
（如 `div.box>ul>li*3`）在 HTML/Vue 里作为补全候选出现，回车展开。

## 启用方式

已在 `init.el` 的 `:my` 段注册：

```elisp
:my
...
web
```

修改后运行 `doom sync` 并重启 Emacs。

## 验证

- `doom sync` 通过，完整配置加载无报错。
- 9 类前端文件均被 lsp-bridge 正确解析到上表对应的 server。
- 各 language server 均能响应 LSP `initialize` 握手。
- Vue 脚本补全已实测：`vtsls + @vue/typescript-plugin` 对 `.vue` 的 `<script>`
  返回真实成员补全（单独 volar 返回 0 条）。
- 生成的 `volar_vtsls_emmet` multiserver 经校验：三个 server 命令均在 PATH、
  插件路径有效。

> 改了本模块后需 `doom sync` 并**重启 Emacs**（首次启动会用你的实际 node 路径
> 重新生成 `<doom-cache>/lsp-bridge-web/` 下的配置），再打开 `.vue` 文件生效。
