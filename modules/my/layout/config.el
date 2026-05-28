;;; my/layout/config.el -*- lexical-binding: t; -*-

;; Custom layout persistence system
(defvar my/layout-directory 
  (expand-file-name "layouts/" 
                    (or (bound-and-true-p doom-user-dir)
                        (bound-and-true-p doom-private-dir)
                        user-emacs-directory))
  "Directory to store saved layouts.")

(defvar my/current-layouts (make-hash-table :test 'equal)
  "Hash table storing current session layouts.")

(defun my/ensure-layout-directory ()
  "Ensure layout directory exists."
  (unless (file-exists-p my/layout-directory)
    (make-directory my/layout-directory t)))

(defun my/layout-file (name)
  "Get the file path for layout NAME."
  (expand-file-name (concat name ".layout") my/layout-directory))

(defun my/get-layout-buffers ()
  "Get alist of (name . file-path) for all buffers in current frame."
  (let (alist)
    (walk-windows
     (lambda (w)
       (let* ((buf (window-buffer w))
              (name (buffer-name buf))
              (path (buffer-file-name buf)))
         (when path
           (push (cons name path) alist))))
     nil t)
    alist))

(defun my/save-layout (name)
  "Save current window configuration as NAME."
  (interactive "sLayout name: ")
  (my/ensure-layout-directory)
  (let ((state (window-state-get (frame-root-window) t))
        (buffers (my/get-layout-buffers)))
    (puthash name (list :state state :buffers buffers) my/current-layouts)
    (with-temp-file (my/layout-file name)
      (prin1 (list :state state :buffers buffers) (current-buffer)))
    (message "Layout '%s' saved" name)))

(defun my/load-layout (name)
  "Load window configuration NAME."
  (interactive
   (let ((layouts (my/list-saved-layouts)))
     (unless layouts
       (user-error "No saved layouts"))
     (list (completing-read "Load layout: " layouts nil t))))
  (let ((file (my/layout-file name))
        data)
    (if (file-exists-p file)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (setq data (read (current-buffer))))
          (error
           (message "Failed to read layout '%s': %s" name (error-message-string err))))
      (setq data (gethash name my/current-layouts)))

    (if data
        (let ((state (if (plist-member data :state) (plist-get data :state) data))
              (buffers (plist-get data :buffers)))
          ;; Restore buffers first so window-state-put can find them by name
          (dolist (pair buffers)
            (let ((buf-name (car pair))
                  (file-path (cdr pair)))
              (unless (get-buffer buf-name)
                (when (file-exists-p file-path)
                  (find-file-noselect file-path)))))
          (condition-case err
              (progn
                (window-state-put state (frame-root-window) 'safe)
                (message "Layout '%s' loaded" name))
            ;; user-error covers "No more buttons" from dashboard re-render
            ((error user-error)
             (message "Failed to restore layout '%s': %s" name (error-message-string err)))))
      (message "Layout '%s' not found" name))))

(defun my/delete-layout (name)
  "Delete saved layout NAME."
  (interactive
   (list (completing-read "Delete layout: " (my/list-saved-layouts) nil t)))
  (let ((file (my/layout-file name)))
    (when (file-exists-p file)
      (delete-file file)
      (remhash name my/current-layouts)
      (message "Layout '%s' deleted" name))))

(defun my/list-saved-layouts ()
  "List all saved layouts."
  (when (file-exists-p my/layout-directory)
    (mapcar (lambda (file)
              (string-remove-suffix ".layout" file))
            (directory-files my/layout-directory nil "\\.layout$"))))

(defun my/list-layouts ()
  "Display all saved layouts."
  (interactive)
  (let ((layouts (my/list-saved-layouts)))
    (if layouts
        (message "Saved layouts: %s" (string-join layouts ", "))
      (message "No saved layouts found"))))

;; Predefined layout templates
(defun my/layout-split-2-vertical ()
  "Create a 2-window vertical split layout."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun my/layout-split-2-horizontal ()
  "Create a 2-window horizontal split layout."
  (interactive)
  (delete-other-windows)
  (split-window-below))

(defun my/layout-split-3-vertical ()
  "Create a 3-window vertical split layout."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun my/layout-split-3-horizontal ()
  "Create a 3-window horizontal split layout."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-below)
  (balance-windows))

(defun my/layout-split-grid ()
  "Create a 2x2 grid layout."
  (interactive)
  (delete-other-windows)
  (split-window-right)   ; W1 left | W2 right, cursor on W1
  (split-window-below)   ; W1 top-left / W3 bottom-left | W2 right, cursor on W1
  (other-window 2)       ; skip W3, jump to W2 (right)
  (split-window-below)   ; W2 top-right / W4 bottom-right
  (balance-windows))

(defun my/layout-split-main-side ()
  "Create a main window with side panel layout (70-30 split)."
  (interactive)
  (delete-other-windows)
  (let ((width (floor (* 0.7 (window-total-width)))))
    (split-window-right width)))

(defun my/layout-reset ()
  "Reset to single window layout."
  (interactive)
  (delete-other-windows)
  (message "Layout reset to single window"))

;; Load all persistent layouts at startup
(defun my/load-all-layouts ()
  "Load all saved layouts into current session."
  (dolist (name (my/list-saved-layouts))
    (let ((file (my/layout-file name)))
      (when (file-exists-p file)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (puthash name (read (current-buffer)) my/current-layouts))
          (error
           (message "Skipping corrupted layout '%s': %s" name (error-message-string err))))))))

;; Auto-save current layout periodically
(defvar my/auto-save-layout-timer nil
  "Timer for auto-saving current layout.")

(defvar my/auto-save-layout-name "my-auto-save"
  "Name for auto-saved layout.")

(defun my/auto-save-layout ()
  "Automatically save current layout."
  (when (> (count-windows) 1)
    (my/save-layout my/auto-save-layout-name)))

(defun my/enable-auto-save-layout ()
  "Enable automatic layout saving every 5 minutes."
  (interactive)
  (unless my/auto-save-layout-timer
    (setq my/auto-save-layout-timer
          (run-with-timer 300 300 #'my/auto-save-layout))
    (message "Auto-save layout enabled")))

(defun my/disable-auto-save-layout ()
  "Disable automatic layout saving."
  (interactive)
  (when my/auto-save-layout-timer
    (cancel-timer my/auto-save-layout-timer)
    (setq my/auto-save-layout-timer nil)
    (message "Auto-save layout disabled")))

;; Restore last layout on startup
(defun my/restore-last-layout ()
  "Restore the auto-saved layout if it exists."
  (when (member my/auto-save-layout-name (my/list-saved-layouts))
    (my/load-layout my/auto-save-layout-name)))

(defun my/window-toggle-maximize ()
  "Toggle maximize current window."
  (interactive)
  (if (one-window-p t)
      (when (bound-and-true-p winner-mode)
        (winner-undo))
    (delete-other-windows)))

;; Initialize on startup
;; (add-hook 'doom-first-input-hook #'my/load-all-layouts)
;; 延迟 0.8s 再恢复布局，避免与 dashboard 的 forward-button 初始化竞争
;; （window-state-put 会触发 dashboard 重绘，此时按钮尚未渲染，导致 "No more buttons"）
;; (add-hook 'doom-first-input-hook
;; (lambda () (run-with-idle-timer 0.8 nil #'my/restore-last-layout)))
(add-hook 'doom-first-input-hook #'my/enable-auto-save-layout)
(add-hook 'kill-emacs-hook #'my/auto-save-layout)

;; Transient menu for layout operations
(after! transient
  (transient-define-prefix my/layout-menu ()
    "Window layout management menu."
    [["Save/Load"
      ("s" "Save layout" my/save-layout)
      ("l" "Load layout" my/load-layout)
      ("d" "Delete layout" my/delete-layout)
      ("L" "List layouts" my/list-layouts)]
     ["Templates"
      ("2v" "2 vertical" my/layout-split-2-vertical)
      ("2h" "2 horizontal" my/layout-split-2-horizontal)
      ("3v" "3 vertical" my/layout-split-3-vertical)
      ("3h" "3 horizontal" my/layout-split-3-horizontal)
      ("g" "Grid 2x2" my/layout-split-grid)
      ("M" "Main+Side" my/layout-split-main-side)]
     ["Actions"
      ("m" "Toggle Maximize" my/window-toggle-maximize)
      ("u" "Undo layout" winner-undo)
      ("r" "Redo layout" winner-redo)
      ("R" "Reset" my/layout-reset)
      ("=" "Balance" balance-windows)
      ("q" "Quit" transient-quit-one)]]))

(map! :leader
      :prefix "l"
      "a" #'my/layout-menu
      "u" #'winner-undo
      "r" #'winner-redo
      "m" #'my/window-toggle-maximize)
