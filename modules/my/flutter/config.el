;;; my/flutter/config.el -*- lexical-binding: t; -*-

;; Doom 的 :lang (dart +flutter +tree-sitter) 已提供 dart-mode / dart-ts-mode / flutter.el。
;; 本模块补齐本机使用 lsp-bridge 时缺少的部分：
;; - dart-ts-mode 接入 dart-analysis-server
;; - 自动发现 Flutter SDK 并补 PATH
;; - Dart / Flutter 常用命令和本地 leader 键

(defvar +flutter-sdk-candidate-dirs
  (delq nil
        (list (getenv "FLUTTER_ROOT")
              (getenv "FLUTTER_HOME")
              (expand-file-name "~/development/flutter")
              (expand-file-name "~/Developer/flutter")
              (expand-file-name "~/dev/flutter")
              (expand-file-name "~/flutter")
              "/opt/flutter"
              "/opt/homebrew/Caskroom/flutter/latest/flutter"))
  "Candidate Flutter SDK roots.")

(defun +flutter--prepend-path (dir)
  "Prepend DIR to `exec-path' and PATH when DIR exists."
  (when (and dir (file-directory-p dir))
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))))

(defun +flutter--sdk-root-from-command ()
  "Return Flutter SDK root from `flutter' executable, if discoverable."
  (when-let* ((exe (executable-find "flutter"))
              (real (file-truename exe)))
    (directory-file-name
     (expand-file-name ".." (file-name-directory real)))))

(defun +flutter--find-sdk-root ()
  "Return the first existing Flutter SDK root."
  (or (+flutter--sdk-root-from-command)
      (cl-find-if (lambda (dir)
                    (and dir
                         (file-executable-p
                          (expand-file-name "bin/flutter" dir))))
                  +flutter-sdk-candidate-dirs)))

(defun +flutter/setup-sdk-paths ()
  "Configure PATH, `flutter-sdk-path' and `dart-sdk-path' from Flutter SDK."
  (when-let* ((root (+flutter--find-sdk-root))
              (bin (expand-file-name "bin" root)))
    (+flutter--prepend-path bin)
    (setq flutter-sdk-path root)
    (let ((dart-sdk (expand-file-name "bin/cache/dart-sdk" root)))
      (when (file-directory-p dart-sdk)
        (setq dart-sdk-path dart-sdk)
        (+flutter--prepend-path (expand-file-name "bin" dart-sdk))))))

(+flutter/setup-sdk-paths)
(after! flutter
  (+flutter/setup-sdk-paths))

(defun +flutter--project-root ()
  "Return nearest Flutter/Dart project root."
  (or (locate-dominating-file default-directory "pubspec.yaml")
      (when (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
      default-directory))

(defun +flutter--compile (command)
  "Run COMMAND from the current Flutter/Dart project root."
  (let ((default-directory (+flutter--project-root)))
    (compile command)))

(defun +flutter/doctor ()
  "Run `flutter doctor -v'."
  (interactive)
  (+flutter--compile "flutter doctor -v"))

(defun +flutter/devices ()
  "List Flutter devices."
  (interactive)
  (+flutter--compile "flutter devices"))

(defun +flutter/pub-get ()
  "Run `flutter pub get'."
  (interactive)
  (+flutter--compile "flutter pub get"))

(defun +flutter/pub-upgrade ()
  "Run `flutter pub upgrade'."
  (interactive)
  (+flutter--compile "flutter pub upgrade"))

(defun +flutter/pub-outdated ()
  "Run `flutter pub outdated'."
  (interactive)
  (+flutter--compile "flutter pub outdated"))

(defun +flutter/test ()
  "Run Flutter tests."
  (interactive)
  (+flutter--compile "flutter test"))

(defun +flutter/build-apk ()
  "Build Android APK."
  (interactive)
  (+flutter--compile "flutter build apk"))

(defun +flutter/build-ios ()
  "Build iOS app."
  (interactive)
  (+flutter--compile "flutter build ios"))

(defun +flutter/build-web ()
  "Build Flutter web app."
  (interactive)
  (+flutter--compile "flutter build web"))

(defun +dart/format-buffer ()
  "Format the current Dart buffer via lsp-bridge when possible."
  (interactive)
  (cond
   ((and (bound-and-true-p lsp-bridge-mode)
         (fboundp 'lsp-bridge-code-format))
    (lsp-bridge-code-format))
   ((buffer-file-name)
    (+flutter--compile
     (format "dart format %s" (shell-quote-argument (buffer-file-name)))))
   (t
    (user-error "当前 buffer 没有关联文件"))))

(defun +flutter/run ()
  "Run the current Flutter app."
  (interactive)
  (if (fboundp 'flutter-run)
      (call-interactively #'flutter-run)
    (+flutter--compile "flutter run")))

(defun +flutter/quit ()
  "Quit the current Flutter process."
  (interactive)
  (if (fboundp 'flutter-quit)
      (call-interactively #'flutter-quit)
    (user-error "flutter.el 尚未加载，无法停止运行中的 Flutter 进程")))

(defun +flutter/hot-reload ()
  "Hot reload the running Flutter app."
  (interactive)
  (if (fboundp 'flutter-hot-reload)
      (call-interactively #'flutter-hot-reload)
    (user-error "flutter.el 尚未加载，无法 hot reload")))

(defun +flutter/hot-restart ()
  "Hot restart the running Flutter app."
  (interactive)
  (if (fboundp 'flutter-hot-restart)
      (call-interactively #'flutter-hot-restart)
    (user-error "flutter.el 尚未加载，无法 hot restart")))

(defun +dart/setup-lsp-bridge ()
  "Enable lsp-bridge in Dart buffers."
  (when (fboundp 'lsp-bridge-mode)
    (lsp-bridge-mode 1)))

(after! lsp-bridge
  ;; lsp-bridge 内置只映射 dart-mode；Doom +tree-sitter 会用 dart-ts-mode。
  (add-to-list 'lsp-bridge-single-lang-server-mode-list
               '((dart-mode dart-ts-mode) . "dart-analysis-server"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'dart-ts-mode-hook)
  (add-to-list 'lsp-bridge-formatting-indent-alist
               '(dart-ts-mode . lsp-bridge-indent-two-level))
  (require 'lsp-bridge-dart nil t))

(add-hook 'dart-mode-hook #'+dart/setup-lsp-bridge)
(add-hook 'dart-ts-mode-hook #'+dart/setup-lsp-bridge)

(after! apheleia
  ;; `dart format --output=show <file>' returns formatted contents on stdout.
  (setf (alist-get 'dart-format apheleia-formatters)
        '("dart" "format" "--output=show" filepath))
  (add-to-list 'apheleia-mode-alist '(dart-mode . dart-format))
  (add-to-list 'apheleia-mode-alist '(dart-ts-mode . dart-format)))

(map! :after dart-mode
      :map dart-mode-map
      :localleader
      "=" #'+dart/format-buffer
      (:prefix ("f" . "flutter")
       "f" #'+flutter/run
       "q" #'+flutter/quit
       "r" #'+flutter/hot-reload
       "R" #'+flutter/hot-restart
       "d" #'+flutter/doctor
       "D" #'+flutter/devices
       "g" #'+flutter/pub-get
       "u" #'+flutter/pub-upgrade
       "o" #'+flutter/pub-outdated
       "t" #'+flutter/test
       "a" #'+flutter/build-apk
       "i" #'+flutter/build-ios
       "w" #'+flutter/build-web))

(map! :after dart-ts-mode
      :map dart-ts-mode-map
      :localleader
      "=" #'+dart/format-buffer
      (:prefix ("f" . "flutter")
       "f" #'+flutter/run
       "q" #'+flutter/quit
       "r" #'+flutter/hot-reload
       "R" #'+flutter/hot-restart
       "d" #'+flutter/doctor
       "D" #'+flutter/devices
       "g" #'+flutter/pub-get
       "u" #'+flutter/pub-upgrade
       "o" #'+flutter/pub-outdated
       "t" #'+flutter/test
       "a" #'+flutter/build-apk
       "i" #'+flutter/build-ios
       "w" #'+flutter/build-web))
