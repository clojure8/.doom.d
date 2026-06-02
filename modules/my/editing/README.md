# my/editing — 编辑小工具

## string-inflection — 命名风格循环切换

对光标处的标识符循环切换命名风格：

```
foo_bar → FOO_BAR → FooBar → fooBar → foo-bar → foo_bar ...
```

Go / JS / Python / Clojure 混用时改名很方便。

| 键 | 命令 | 作用 |
|----|------|------|
| `SPC c ~` | `string-inflection-all-cycle` | 循环全部风格 |
| `SPC c _` | `string-inflection-toggle` | 下划线 ↔ 驼峰快速切换 |

不改动 evil 默认的 `g~`（swap-case）。
