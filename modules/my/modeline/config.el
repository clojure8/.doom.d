;;; my/modeline/config.el -*- lexical-binding: t; -*-

;; ── doom-modeline 瘦身：高度按字体行高比例（可配置）──────────────────────
;; 改 `+modeline-height-ratio' 调高度：1.0=与字体等高（最瘦），1.1~1.3 留点留白。
;; 改后 `M-x +modeline/apply-height' 即时生效（或重启）。
(defcustom +modeline-height-ratio 1.0
  "doom-modeline 高度相对字体行高 `frame-char-height' 的倍数。"
  :type 'number
  :group 'doom-modeline
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp '+modeline/apply-height) (+modeline/apply-height))))

(defun +modeline/apply-height (&rest _)
  "按 `+modeline-height-ratio' 把 doom-modeline 高度设成字体行高的比例。"
  (interactive)
  (when (and (display-graphic-p) (boundp 'doom-modeline-height))
    (setq doom-modeline-height
          (max 1 (round (* +modeline-height-ratio (frame-char-height)))))
    (when (fboundp 'doom-modeline-refresh-bars)
      (doom-modeline-refresh-bars))))

(after! doom-modeline
  (+modeline/apply-height))
;; 字体 / 主题 / 新建 frame（daemon 首帧）变化时重算高度
(add-hook 'after-setting-font-hook      #'+modeline/apply-height)
(add-hook 'doom-load-theme-hook         #'+modeline/apply-height)
(add-hook 'server-after-make-frame-hook #'+modeline/apply-height)
