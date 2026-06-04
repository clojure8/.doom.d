;;; my/aigen/config.el -*- lexical-binding: t; -*-

;; 基于 gptel 的若干「就地生成」AI 命令：
;;   1. +ai/implement-from-comment  根据注释/选区生成代码，插到注释下方
;;   2. +ai/summarize-region        总结选中内容，插到选区下方
;;   3. +ai/expand-region           根据选中的描述/提纲扩展成正文
;;   4. +ai/prompt-region           对选区执行任意自定义指令
;;
;; 复用 my/gptel 里配置好的后端/模型（默认智谱 glm-5.1），异步请求、回调插入。

(defvar +ai-implement-system
  "You are an expert programmer. The user gives you a code comment describing what to implement. Reply with ONLY the code snippet that implements it, meant to be inserted directly below the comment inside an EXISTING file at that exact location. Output just the implementation fragment (e.g. the function body, function, or expression the comment asks for). Do NOT add a `main`/entry-point function, package/module/namespace declarations, import or `use`/`require` statements, example usage, tests, or any other scaffolding or boilerplate — unless the comment explicitly asks for it. Assume necessary imports and surrounding context already exist. No explanations, no markdown code fences, no surrounding prose. Write idiomatic %s and match common conventions. Do not repeat the comment."
  "实现代码用的 system 提示，%s 会被替换成语言名。")

;; ── 工具函数 ────────────────────────────────────────────────────────────

(defun +ai--strip-fences (text)
  "去掉 TEXT 外层的 markdown 代码围栏（``` 或 ~~~），若存在。"
  (let ((s (string-trim text)))
    (if (string-match
         "\\`\\(?:```\\|~~~\\)[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?\\(?:```\\|~~~\\)[ \t]*\\'"
         s)
        (string-trim-right (match-string 1 s))
      s)))

(defun +ai--lang-name ()
  "由 major-mode 推出大致的语言名。"
  (let ((m (symbol-name major-mode)))
    (capitalize
     (replace-regexp-in-string "\\(?:-ts\\)?-mode\\'" "" m))))

(defun +ai--busy (what)
  "提示正在请求。"
  (message "%s…（%s · %s）" what
           (ignore-errors (gptel-backend-name gptel-backend)) gptel-model))

(defun +ai--insert-below (marker text &optional reindent blank-line)
  "在 MARKER 所在行的下一行插入 TEXT。
REINDENT 非空则对插入区域执行 `indent-region'；
BLANK-LINE 非空则在前面多空一行。"
  (let ((buf (marker-buffer marker)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char marker)
          (goto-char (line-end-position))
          (let ((start (point)))
            (insert (if blank-line "\n\n" "\n") text)
            (when reindent
              (ignore-errors (indent-region start (point))))
            (goto-char start)))
        (message "AI 已插入结果。")))))

(defun +ai--request (input system marker &optional reindent blank-line transform)
  "向 LLM 发送 INPUT（system 为 SYSTEM），结果插到 MARKER 下方。
TRANSFORM 非空时先对响应文本做一次变换。"
  (gptel-request input
    :system system
    :callback
    (lambda (resp info)
      (if (stringp resp)
          (+ai--insert-below
           marker
           (funcall (or transform #'string-trim) resp)
           reindent blank-line)
        (message "AI 请求失败：%s" (or (plist-get info :status) "无响应"))))))

(defun +ai--region-bounds-or-error (what)
  "返回激活选区的 (BEG . END)，未选区则报错，提示 WHAT。"
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (user-error "请先选择%s" what)))

;; ── 命令 ────────────────────────────────────────────────────────────────

;;;###autoload
(defun +ai/implement-from-comment ()
  "根据当前注释行（或激活选区）生成代码，插入到注释下方。"
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (cons (line-beginning-position) (line-end-position))))
         (text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (lang (+ai--lang-name))
         (marker (copy-marker (cdr bounds))))
    (when (string-empty-p text)
      (user-error "注释为空：把光标放在注释行上，或选中注释块"))
    (+ai--request text (format +ai-implement-system lang) marker
                  'reindent nil #'+ai--strip-fences)
    (+ai--busy "AI 正在根据注释生成代码")))

;;;###autoload
(defun +ai/summarize-region ()
  "总结选中内容，把摘要插到选区下方。"
  (interactive)
  (let* ((b (+ai--region-bounds-or-error "要总结的内容"))
         (text (buffer-substring-no-properties (car b) (cdr b)))
         (marker (copy-marker (cdr b))))
    (+ai--request
     text
     "你是简洁的总结助手。用与输入相同的语言，对用户内容做要点式总结，抓住要点、去掉冗余。只输出总结本身，不要寒暄或前后缀。"
     marker nil 'blank-line
     (lambda (r) (concat "【摘要】\n" (string-trim r))))
    (+ai--busy "AI 正在总结选中内容")))

;;;###autoload
(defun +ai/expand-region ()
  "根据选中的描述/提纲扩展成结构清晰的正文，插到选区下方。"
  (interactive)
  (let* ((b (+ai--region-bounds-or-error "要扩展的描述"))
         (text (buffer-substring-no-properties (car b) (cdr b)))
         (marker (copy-marker (cdr b))))
    (+ai--request
     text
     "你是写作助手。把用户给的简短描述或提纲，扩展成结构清晰、内容充实、可直接使用的正文，使用与输入相同的语言。只输出扩展后的正文，不要解释你做了什么。"
     marker nil 'blank-line)
    (+ai--busy "AI 正在扩展内容")))

;;;###autoload
(defun +ai/prompt-region (instruction)
  "对选区内容执行任意 INSTRUCTION，结果插到选区下方。"
  (interactive (list (read-string "对选区执行的指令: ")))
  (let* ((b (+ai--region-bounds-or-error "要处理的内容"))
         (text (buffer-substring-no-properties (car b) (cdr b)))
         (marker (copy-marker (cdr b))))
    (when (string-empty-p (string-trim instruction))
      (user-error "指令为空"))
    (+ai--request
     text
     (format "你是文本/代码助手。按用户指令处理其提供的内容，使用与输入相同的语言。只输出结果本身。指令：%s" instruction)
     marker nil 'blank-line)
    (+ai--busy "AI 正在按指令处理")))

;; ── 键绑定：挂到 Doom 的 `SPC o l'（llm）下的 `g'(ai-gen) 子前缀 ──────────
(after! gptel
  (map! :leader
        (:prefix ("o" . "open")
         (:prefix ("l" . "llm")
          (:prefix ("g" . "ai-gen")
           :desc "注释→代码"   "c" #'+ai/implement-from-comment
           :desc "总结选区"     "s" #'+ai/summarize-region
           :desc "扩展描述"     "e" #'+ai/expand-region
           :desc "自定义指令"   "p" #'+ai/prompt-region)))))
