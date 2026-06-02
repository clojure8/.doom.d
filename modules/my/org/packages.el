;;; my/org/packages.el -*- lexical-binding: t; -*-

(package! valign
  :recipe (:host github :repo "casouri/valign"))
(package! org-appear)

;; agenda 分组显示
(package! org-super-agenda)
;; org → GitHub 风格 markdown 导出
(package! ox-gfm)
;; 把 docx / markdown / ... 导入成 org（依赖 pandoc）
(package! org-pandoc-import
  :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))
;; org-roam 的可视化 Web UI（org-roam 本体由 Doom :lang org +roam2 提供）
(package! org-roam-ui)
