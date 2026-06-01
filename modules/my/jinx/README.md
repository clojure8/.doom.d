# my/jinx — 拼写检查（jinx + enchant）

[`jinx`](https://github.com/minad/jinx)：基于 enchant 的现代即时拼写检查，
首次加载时用 `cc` + `pkg-config` 把 `jinx-mod.c` 编成动态模块。

## 之前的问题与修复

`jinx` 一开始在 `init.el` 里是注释禁用状态。原因是：`global-jinx-mode` 用
`en_US` 词典会把**中文（CJK）整段误标为拼写错误**——满屏中文都被画波浪线。

`config.el` 已修复：往 `jinx-exclude-regexps` 加了一条覆盖 CJK 各区段（部首、
符号标点、假名、注音、扩展A、统一汉字、谚文、兼容汉字、全角/半角）的正则，
让 jinx 跳过 CJK，只查英文。已实测：

- `混合 Chinese 和 a misspeled word 的句子。` → 只标 `misspeled`
- 纯中文句子 → 零误报
- 英文句子 → 拼写错误照常标出

## 依赖

- `enchant`（libenchant-2，含后端如 aspell/hunspell/AppleSpell）：`brew install enchant`
- `pkg-config`、C 编译器（`cc`/`clang`）
- 词典：`en_US`（本机经 aspell 提供）

## 用法

`global-jinx-mode` 已随 `after-init` 自动开启。光标在被标记的词上：

| 键 | 作用 |
|----|------|
| `M-$` | 纠正当前词（`jinx-correct`） |
| `C-u M-$` | 纠正全 buffer |
| `M-n` / `M-p` | 跳到下/上一个拼写错误 |
