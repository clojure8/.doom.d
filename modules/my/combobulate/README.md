# my/combobulate — tree-sitter 结构化导航/编辑

你几乎所有 `:lang` 都开了 `+tree-sitter`，但默认只用它做语法高亮。
[combobulate](https://github.com/mickeynp/combobulate) 把语法树真正用起来：

- **按节点导航**：上一个/下一个兄弟、进入父节点、进入子节点
- **拖动节点**：把一个语句/表达式整体上移下移
- **结构化操作**：按语法选区、删除、克隆、splice、标记
- **智能编辑**：在正确的语法位置插入、包裹

## 用法

进入支持的 `*-ts-mode` 后，前缀键 `C-c o`（`combobulate-key-prefix`）：

- `C-c o o` 打开 combobulate 主菜单（transient，列全部命令）
- 之后按提示操作即可

## 覆盖的模式

`python-ts` / `js-ts` / `typescript-ts` / `tsx-ts` / `json-ts` / `css-ts` /
`yaml-ts` / `html-ts` / `go-ts`。需要对应 tree-sitter grammar 已安装（你已开
`treesit-auto-install`）。
