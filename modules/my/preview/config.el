;;; my/preview/config.el -*- lexical-binding: t; -*-

;; 为 org / markdown 提供实时 HTML 预览：
;;   - 优先 xwidget（内嵌 WebKit，分屏显示，保存后自动重载）
;;   - 回退默认浏览器（file:// 打开，保存后重新打开）
;;
;; 导出：org 用内置 ox-html；markdown 直接调 pandoc（-f gfm -t html5 -s），
;; 不依赖 grip。HTML 写在源文件同目录的隐藏文件里，相对图片/链接照常解析。

(defcustom +preview-pandoc-args
  '("-f" "gfm" "-t" "html5" "-s" "--highlight-style=tango")
  "markdown 预览调用 pandoc 的参数。"
  :type '(repeat string) :group 'my-preview)

;; 声明这些 org 导出变量为特殊变量，以便在本 lexical-binding 文件里 `let' 动态绑定
;; （否则 Emacs 30 在 org/ox-html 尚未把它们标记为 special 时会报
;; "Defining as dynamic an already lexical var"）。
(defvar org-export-use-babel)
(defvar org-html-validation-link)

(defvar-local +preview--method nil "本 buffer 预览方式：xwidget 或 browser。")
(defvar-local +preview--active nil "本 buffer 是否已开启预览刷新。")
(defvar-local +preview--tmp nil "非文件 buffer 用的临时 HTML 路径。")
(defvar +preview--xwidget nil "复用的 xwidget-webkit 会话。")

;; ── 导出 ──────────────────────────────────────────────────────────────────
(defun +preview--html-path ()
  "返回本 buffer 预览 HTML 的输出路径。
有文件名时写到源文件同目录的 `.NAME.preview.html'（相对资源可解析）；
否则用一个稳定的临时文件。"
  (if buffer-file-name
      (expand-file-name (format ".%s.preview.html" (file-name-base buffer-file-name))
                        (file-name-directory buffer-file-name))
    (or +preview--tmp
        (setq +preview--tmp (make-temp-file "emacs-preview-" nil ".html")))))

(defun +preview--export (out)
  "把当前 buffer 导出为 HTML 到 OUT，返回 OUT。"
  (cond
   ((derived-mode-p 'org-mode)
    (let ((org-export-use-babel nil)            ; 预览不跑代码块：快且安全
          (org-html-validation-link nil))
      (org-export-to-file 'html out)))
   ((derived-mode-p 'markdown-mode)
    (let ((default-directory (if buffer-file-name
                                 (file-name-directory buffer-file-name)
                               default-directory))
          (title (format "preview: %s" (buffer-name))))
      (unless (executable-find "pandoc")
        (user-error "markdown 预览需要 pandoc（brew install pandoc）"))
      (let ((status (apply #'call-process-region (point-min) (point-max) "pandoc"
                           nil `((:file ,out) nil) nil
                           (append +preview-pandoc-args
                                   (list "--metadata" (concat "title=" title))))))
        (unless (eq status 0)
          (user-error "pandoc 导出失败（退出码 %s）" status)))))
   (t (user-error "只支持 org / markdown buffer")))
  out)

;; ── 显示 ──────────────────────────────────────────────────────────────────
(defun +preview--file-url (path)
  (concat "file://" (url-encode-url (expand-file-name path))))

(defun +preview--show-xwidget (html)
  "在右侧分屏的 xwidget-webkit 里显示 HTML（已存在则原地重载）。"
  (unless (and (display-graphic-p) (featurep 'xwidget-internal))
    (user-error "xwidget 不可用：需 GUI frame + 编译了 --with-xwidgets 的 Emacs"))
  (require 'xwidget)
  (let ((url (+preview--file-url html)))
    (if (and +preview--xwidget (ignore-errors (xwidget-live-p +preview--xwidget)))
        ;; 复用会话：导航到（同一）URL 即重载
        (xwidget-webkit-goto-uri +preview--xwidget url)
      (let ((src (selected-window)))
        (select-window (or (window-in-direction 'right) (split-window-right)))
        (xwidget-webkit-browse-url url t)
        (setq +preview--xwidget (xwidget-webkit-current-session))
        (when (window-live-p src) (select-window src))))))

(defun +preview--show-browser (html)
  (browse-url (+preview--file-url html)))

(defun +preview--render ()
  "导出并按当前方式显示。"
  (let ((html (+preview--export (+preview--html-path))))
    (pcase +preview--method
      ('xwidget (+preview--show-xwidget html))
      (_        (+preview--show-browser html)))))

(defun +preview--after-save ()
  (when +preview--active (ignore-errors (+preview--render))))

(defun +preview--cleanup ()
  "kill buffer 时删掉生成的预览 HTML。"
  (let ((f (+preview--html-path)))
    (when (and f (file-exists-p f) (string-match-p "\\.preview\\.html\\'" f))
      (ignore-errors (delete-file f)))))

;; ── 命令 ──────────────────────────────────────────────────────────────────
;;;###autoload
(defun +preview/open (&optional method)
  "实时预览当前 org/markdown buffer。METHOD 为 nil 时自动选 xwidget/browser。
保存后自动刷新。"
  (interactive)
  (unless (derived-mode-p 'org-mode 'markdown-mode)
    (user-error "只支持 org / markdown buffer"))
  (setq +preview--method
        (or method
            (if (and (display-graphic-p) (featurep 'xwidget-internal)) 'xwidget 'browser))
        +preview--active t)
  (+preview--render)
  (add-hook 'after-save-hook #'+preview--after-save nil t)
  (add-hook 'kill-buffer-hook #'+preview--cleanup nil t)
  (message "预览已开启（%s）：保存后自动刷新，`+preview/stop' 停止" +preview--method))

;;;###autoload
(defun +preview/xwidget () "用 xwidget 预览。" (interactive) (+preview/open 'xwidget))
;;;###autoload
(defun +preview/browser () "用默认浏览器预览。" (interactive) (+preview/open 'browser))

;;;###autoload
(defun +preview/stop ()
  "停止本 buffer 的预览自动刷新。"
  (interactive)
  (setq +preview--active nil)
  (remove-hook 'after-save-hook #'+preview--after-save t)
  (message "预览刷新已停止"))

;; ── 键绑定：org / markdown 的 localleader 下 `v'(view/preview) 子前缀 ───────
(after! org
  (map! :map org-mode-map :localleader
        (:prefix ("v" . "preview")
         :desc "预览(自动)"   "v" #'+preview/open
         :desc "xwidget 预览" "x" #'+preview/xwidget
         :desc "浏览器预览"   "b" #'+preview/browser
         :desc "停止刷新"     "q" #'+preview/stop)))

(after! markdown-mode
  (map! :map (markdown-mode-map gfm-mode-map) :localleader
        (:prefix ("v" . "preview")
         :desc "预览(自动)"   "v" #'+preview/open
         :desc "xwidget 预览" "x" #'+preview/xwidget
         :desc "浏览器预览"   "b" #'+preview/browser
         :desc "停止刷新"     "q" #'+preview/stop)))
