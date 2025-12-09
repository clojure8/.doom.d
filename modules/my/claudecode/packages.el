;; -*- no-byte-compile: t; -*-
;;; my/ai/packages.el


;; 声明 monet (Git 仓库依赖)
(package! monet
  :recipe (:host github
           :repo "stevemolitor/monet"
           :branch "main" ; 或其他分支, :rev :newest 通常对应主分支
           :files ("*.el")))

;; 声明 inheritenv (Git 仓库依赖)
(package! inheritenv
  :recipe (:host github
           :repo "purcell/inheritenv"
           :branch "main" ; 需根据实际情况确认分支名
           :files ("*.el")))

;; 声明 claude-code (Git 仓库依赖)
(package! claude-code
  :recipe (:host github
           :repo "stevemolitor/claude-code.el"
           :branch "main"
           :files ("*")))
