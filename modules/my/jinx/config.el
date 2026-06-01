;; -*- no-byte-compile: t; -*-
;;; my/jinx/config.el


;; jinx：基于 enchant 的现代拼写检查（首次加载会用 cc + pkg-config 编译
;; jinx-mod 动态模块；enchant 经 aspell 后端检查 en_US）。
(use-package! jinx
  :hook (after-init . global-jinx-mode)
  :config
  (setq jinx-languages "en_US")

  ;; 跳过 CJK（中日韩）字符。否则 global-jinx-mode 会拿 en_US 词典去查中文，
  ;; 把整段中文/日文/韩文全部误标为拼写错误（这正是之前禁用 jinx 的原因）。
  ;; 覆盖：CJK 部首补充、符号与标点、假名、注音、CJK 扩展A、CJK 统一汉字、
  ;; 谚文音节、CJK 兼容汉字、全角/半角形式等区段。英文拼写检查不受影响。
  (add-to-list 'jinx-exclude-regexps
               '(t "[⺀-⻿　-〿぀-ヿ㄀-ㄯ㐀-䶿一-鿿가-힯豈-﫿＀-￯]+")))

