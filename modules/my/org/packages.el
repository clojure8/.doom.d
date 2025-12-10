;;; my/org/packages.el -*- lexical-binding: t; -*-

;; 自定义 org 模块的包管理

;; 添加 valign 包支持，用于表格对齐
(package! valign
  :recipe (:host github :repo "casouri/valign"))


(package! org-superstar)
