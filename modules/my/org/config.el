;;; my/org/config.el -*- lexical-binding: t; -*-

(when (modulep! +pretty)
  (load! "+pretty"))

(after! org
  ;; TODO 关键字
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w)" "|" "DONE(d)" "KILL(k)"))
        org-todo-keyword-faces
        '(("NEXT" . +org-todo-active)
          ("PROG" . "orange")
          ("WAIT" . +org-todo-onhold)))

  ;; Agenda
  (setq org-agenda-files '("~/org/")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t)

  ;; Capture 模板
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/inbox.org")
           "* TODO %?\n  %U\n" :empty-lines 1)
          ("n" "Note" entry (file "~/org/notes.org")
           "* %? :note:\n  %U\n" :empty-lines 1)))

  ;; 阅读体验：打开即显示内嵌图片；嵌套列表 bullet 轮换，层级更清晰
  (setq org-startup-with-inline-images t
        org-list-demote-modify-bullet '(("-" . "+") ("+" . "*") ("*" . "-"))
        ;; 复选框统计联动到父级（[2/4] / [50%]）
        org-checkbox-hierarchical-statistics nil)

  ;; 代码块
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

;; ── Jupyter（数据科学：org-babel + REPL）─────────────────────────────────
;; 运行时：jupyter CLI（pyenv shims）+ python3 kernel（ipykernel，pyenv 3.12.2，
;; 已装 numpy/pandas/matplotlib）。Doom :lang org +jupyter 已接好 ob-jupyter /
;; jupyter-repl，这里只补默认 header 参数与就地求值体验。
;;
;; 写代码块：#+begin_src jupyter-python ... ；C-c C-c 执行（异步，结果含图自动内联）。
(after! jupyter
  ;; 非 org 缓冲区里 jupyter-eval（SPC m e 等）结果以 overlay 就地显示
  (setq jupyter-eval-use-overlays t))

(after! ob-jupyter
  ;; jupyter-python 代码块默认：异步、共享名为 py 的 session、python3 kernel、
  ;; 导出时代码+结果都带上
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")
          (:kernel . "python3")
          (:exports . "both"))))

;; ── org-super-agenda：agenda 分组显示 ─────────────────────────────────────
(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:name "⏰ 今天到期" :deadline today :scheduled today)
          (:name "🔥 进行中"   :todo "PROG")
          (:name "➡️ 下一步"   :todo "NEXT")
          (:name "⚠️ 已逾期"   :deadline past :scheduled past)
          (:name "⏳ 等待"     :todo "WAIT")
          (:name "📌 重要"     :priority "A")
          (:name "🗓 即将"     :deadline future :scheduled future)))
  (org-super-agenda-mode 1))

;; ── ox-gfm：org → GitHub 风格 markdown 导出（C-c C-e g g/G）────────────────
(after! ox
  (require 'ox-gfm nil t))

;; ── org-pandoc-import：把 docx / markdown / rst 等导入成 org ───────────────
(use-package! org-pandoc-import :after org)

;; ── org-roam Web UI（org-roam 本体由 Doom :lang org +roam2 提供）───────────
;; org-roam-directory 用 Doom +roam2 的默认值（org-roam 加载时算成 ~/org/roam/，
;; 此处不提前 setq，否则 org-directory 尚未就绪会算成 ~/roam/）。
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))
