;;; my/editing/config.el -*- lexical-binding: t; -*-

;; string-inflection：对光标处的标识符循环切换命名风格
;;   foo_bar → FOO_BAR → FooBar → fooBar → foo-bar → foo_bar ...
;; 编程语言混用（Go/JS/Python/Clojure）时改名很顺手。

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-upcase)
  :init
  ;; 不动 evil 的 g~（改大小写），只挂 leader 键
  (map! :leader
        (:prefix ("c" . "code")
         :desc "循环命名风格" "~" #'string-inflection-all-cycle
         :desc "下划线↔驼峰"  "_" #'string-inflection-toggle)))
