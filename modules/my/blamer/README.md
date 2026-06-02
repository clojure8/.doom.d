# my/blamer — 行内 git blame

[blamer](https://github.com/Artawower/blamer.el) 让光标停在某行约 0.5s 后，行尾
以淡色斜体显示该行的 `作者 · 相对时间 · 提交信息`，不用专门开 `magit-blame`。

默认通过 `global-blamer-mode` 全局开启（仅对 git 跟踪的文件生效）。

## 调整

- `blamer-idle-time`：触发延迟（默认 0.5s）
- `blamer-min-offset`：blame 文本与代码的最小间距
- `blamer-max-commit-message-length`：提交信息截断长度（默认 60）
- 临时关闭某 buffer：`M-x blamer-mode`

## 依赖

- `git`（已安装）
