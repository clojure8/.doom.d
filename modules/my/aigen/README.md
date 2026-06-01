# my/aigen — gptel 就地 AI 生成命令

基于 [`my/gptel`](../gptel) 配好的后端/模型（默认智谱 `glm-5.1`），提供几个
「就地生成、回调插入」的 AI 命令。请求是异步的（gptel 用 curl），结果回来后
插到合适位置，不打断编辑。

## 命令

| 命令 | 作用 | 输入 | 结果插入位置 |
|------|------|------|--------------|
| `+ai/implement-from-comment` | 根据注释生成代码 | 当前注释行 / 选区 | 注释**下方**，按 major-mode 重新缩进，自动去掉 markdown 代码围栏 |
| `+ai/summarize-region` | 总结选中内容 | 选区 | 选区下方，加 `【摘要】` 抬头 |
| `+ai/expand-region` | 把描述/提纲扩展成正文 | 选区 | 选区下方 |
| `+ai/prompt-region` | 对选区执行任意自定义指令 | 选区 + 指令 | 选区下方 |

所有命令都用「与输入相同的语言」回复，且只输出结果本身（提示词里已约束）。

## 键绑定

挂在 Doom 既有的 `SPC o l`（llm）菜单下新增的 `g`（ai-gen）子前缀：

| 键 | 命令 |
|----|------|
| `SPC o l g c` | 注释→代码 |
| `SPC o l g s` | 总结选区 |
| `SPC o l g e` | 扩展描述 |
| `SPC o l g p` | 自定义指令 |

也可直接 `M-x +ai/...`。

## 依赖

- `my/gptel`：提供 `gptel` 及后端（智谱 / DeepSeek）和 API key（环境变量
  `GPTEL_ZHIPU_API_KEY` / `GPTEL_DEEPSEEK_API_KEY`）。
- `curl`：gptel 默认用它发请求（也能穿过本机代理）。

切换模型/后端用 gptel 自身的 `SPC o l m`（gptel 菜单）即可，本模块跟随
`gptel-backend` / `gptel-model`。

## 验证

四个命令均已用真实 LLM 实测通过：

- 注释 `;; 返回两个数的最大值` → 生成 `(defun max-of-two (a b) ... (max a b))` 并插到注释下方；
- 选中一段介绍 → 生成要点式 `【摘要】`；
- 选中「提纲：番茄工作法三步骤」→ 扩展成结构化正文；
- 选中英文 + 指令「翻译成中文」→ 输出中文译文。
