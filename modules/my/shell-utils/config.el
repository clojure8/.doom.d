;;; my/shell-utils/config.el -*- lexical-binding: t; -*-

;; macOS「用指定应用打开」命令（在 dired 中使用）
(use-package! dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (defun dwim-shell-commands-macos-open-with ()
    "Open marked files with a chosen macOS application."
    (interactive)
    (let* ((apps (seq-sort
                  #'string-lessp
                  (seq-mapcat (lambda (paths)
                                (directory-files-recursively
                                 paths "\\.app$" t (lambda (path)
                                                     (not (string-suffix-p ".app" path)))))
                              '("/Applications" "~/Applications" "/System/Applications"))))
           (selection (progn
                        (cl-assert apps nil "No apps found")
                        (completing-read "Open with: "
                                         (mapcar (lambda (path)
                                                   (propertize (file-name-base path) 'path path))
                                                 apps)))))
      (dwim-shell-command-on-marked-files
       "Open with"
       (format "open -a '%s' '<<*>>'" (get-text-property 0 'path selection))
       :silent-success t
       :no-progress t
       :utils "open"))))
