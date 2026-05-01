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

  ;; 代码块
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))
