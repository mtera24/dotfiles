;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; Now you can use leaf!
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; set hide/show folding mode
(leaf *hs-minor-mode-setting
;;  :bind (("C-#" . hs-toggle-hiding))
  :config
  (add-hook 'c++-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1)))
  (add-hook 'c-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1)))
  (add-hook 'scheme-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1)))
  (add-hook 'emacs-lisp-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1)))
  (add-hook 'lisp-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1)))
  (add-hook 'python-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1)))
  (add-hook 'ruby-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1)))
  (add-hook 'xml-mode-hook
	    '(lambda nil
	       (hs-minor-mode 1))))

(leaf *standard-configuration
  :config
  ;; font setting
  ;; font (Ladicleさん)
  (leaf *font-settings
    :when window-system
    :setq ((use-default-font-for-symbols)
	   (inhibit-compacting-font-caches . t)
	   (jp-font-family . "Cica")
	   (default-font-family . "Cica"))
    :config
    (defun set-japanese-font (family)
      (set-fontset-font
       (frame-parameter nil 'font)
       'japanese-jisx0208
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       'japanese-jisx0212
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       'katakana-jisx0201
       (font-spec :family family)))

    (defun set-latin-and-greek-font (family)
      (set-fontset-font
       (frame-parameter nil 'font)
       '(592 . 687)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(160 . 255)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(256 . 383)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(384 . 591)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(8216 . 8217)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(9608 . 9608)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(9472 . 9472)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(9476 . 9599)
       (font-spec :family family))
      (set-fontset-font
       (frame-parameter nil 'font)
       '(880 . 1023)
       (font-spec :family family)))

    (when (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :family jp-font-family :height 150))
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family jp-font-family :height 140))
    (when (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :family jp-font-family :height 150))
    (set-japanese-font jp-font-family)
    (set-latin-and-greek-font default-font-family)
    (add-to-list 'face-font-rescale-alist
		 (cons default-font-family 0.86))
    (add-to-list 'face-font-rescale-alist
		 (cons jp-font-family 1.0)))
  (leaf *display-settings
    :config
    (toggle-scroll-bar 0)
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (when (eq system-type 'windows-nt)
      (setq initial-frame-alist
	    '((top . 0) (left . 1910) (width . 126) (height . 68))))
    )
  ;;# doom modeline
  (leaf doom-modeline
    :hook (after-init-hook)
    :custom ((doom-modeline-buffer-file-name-style quote truncate-with-project)
	     (doom-modeline-icon . t)
	     (doom-modeline-major-mode-icon . t)
	     (doom-modeline-minor-modes))
    :config
    (with-eval-after-load 'doom-modeline
      (doom-modeline-def-segment evil-state "The current evil state.  Requires `evil-mode' to be enabled."
				 (when (bound-and-true-p evil-local-mode)
				   (s-trim-right
				    (evil-state-property evil-state :tag t))))
      (line-number-mode 0)
      (column-number-mode 0)
      (doom-modeline-def-modeline 'main
				  '(bar workspace-name window-number evil-state matches buffer-info remote-host buffer-position parrot selection-info)
				  '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))
 
  )
  

;;set theme at last
(leaf doom-themes
  :ensure t
  :custom ((doom-themes-enable-italic . t)
	   (doom-themes-enable-bold . t))
  :custom-face ((doom-modeline-bar quote
				   ((t
				     (:background "#6272a4")))))
  :require t
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; end of setting with leaf
(provide 'init)

;; ;; 行間を指定
;; ;;(setq-default line-spacing 0.2)



;; ;; Default Encoding
;; ;;
;; (set-language-environment 'Japanese)
;; ;;
;; (set-keyboard-coding-system 'utf-8)
;; ;; coding-systemで自動的に文字コードを決定する際の優先するコードリストを設定する。
;; (setq buffer-file-coding-system 'utf-8-unix)
;; (prefer-coding-system 'utf-8-unix)

;; ;; 表示を単純化（スタートアップメッセージなし．スクラッチは空．ツールバー無し，
;; ;; スクロールバーなし）．，ベル無し．
;; (setq ring-bell-function 'ignore)
;; (setq inhibit-startup-message t)
;; (setq initial-scratch-message "") 

;; (setq frame-title-format (format "%%f - Emacs@%s" (system-name)))


;; 
;; ;;;# ddskk
;; ;; load-path の設定
;; ;; ----------------
;; ;; 次のとおり 変数 load-path を設定してください。既にパスが通っているならば
;; ;; 不要です。
;; ;;   (setq load-path (cons "c:/emacs-26.3-x86_64/share/emacs/site-lisp/skk" load-path))

;; ;; info ディレクトリの設定
;; ;; -----------------------
;; ;; 次のとおり 変数 Info-default-directory-list を設定してください。既にパス
;; ;; が通っているならば不要です。
;; ;;   (setq Info-default-directory-list
;; ;;         (cons "c:/emacs-26.3-x86_64/share/info" Info-default-directory-list))

;; ;;(use-package ddskk)
;;   ;; skkを標準の入力方法に
;;   (setq default-input-method "japanese-skk")


;; 
;; ;;;# for org-mode
;;   ;; TODO状態
;;   (setq org-todo-keywords
;;         '((sequence "APPT(a@/!)" "TODO(t)" "STARTED(s!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCEL(c@/!)" "SOMEDAY(s@/!)"))
;;         )
;;   ;; DONEの時刻を記録
;;   (setq org-log-done 'time)
;;   ;; アジェンダ表示の対象ファイル
;;   (setq org-agenda-files '(
;;                            "~/org/gtd.org"
;;                 ;;           "~/org/active"
;;                            )
;;         )
;;   ;; refile target
;;   (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
;;   ;; アジェンダ表示で下線を用いる
;;   (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
;;   (setq hl-line-face 'underline)
;;   ;; 標準の祝日を利用しない
;;   (setq calendar-holidays nil)
;;   ;; org-capture 構成
;;  (setq org-capture-templates
;;        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
;;           "* TODO %? (wrote on %U)")
;;          ("k" "Knowledge" entry (file+headline "~/org/knowledge.org" "Inbox")
;;           "* %?  # Wrote on %U")
;;          )
;;  )

;; 
;; ;;;#settings by packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ;;;#persp
;; (use-package persp-mode
;;   :disabled
;;   :diminish
;;   :commands (get-current-persp persp-contain-buffer-p)
;;   :hook (after-init . persp-mode))

;; 
;; ;;;#all-the-icons
;; (use-package all-the-icons
;;   :custom
;;   (all-the-icons-scale-factor 1.0))
  
;; 
;; ;;;#emoji (for the following themes)
;; (use-package emojify :ensure t
;;   :if (display-graphic-p)
;;   :hook (after-init . global-emojify-mode)
;;   :bind
;;   ("C-x e" . 'emojify-insert-emoji)
;;   )

;; 
;; ;;;#which-key
;; (use-package which-key
;;     :diminish which-key-mode
;;     :hook (after-init . which-key-mode))

;; 
;; ;;;#evil-mode
;; (use-package evil)
;; 
;; ;;;#undo-tree
;; (use-package undo-tree
;;   :bind
;;   ("M-/" . undo-tree-redo)
;;   :config
;;   (global-undo-tree-mode))

;; ;;;#theme
;; ;;;# doom theme

;; 
;; 
;; ;;;#task-measuring
;; (defun ladicle/task-clocked-time ()
;;         "Return a string with the clocked time and effort, if any"
;;         (interactive)
;;         (let* ((clocked-time (org-clock-get-clocked-time))
;;                (h (truncate clocked-time 60))
;;                (m (mod clocked-time 60))
;;                (work-done-str (format "%d:%02d" h m)))
;;           (if org-clock-effort
;;               (let* ((effort-in-minutes
;;                   (org-duration-to-minutes org-clock-effort))
;;                  (effort-h (truncate effort-in-minutes 60))
;;                  (effort-m (truncate (mod effort-in-minutes 60)))
;;                  (effort-str (format "%d:%02d" effort-h effort-m)))
;;             (format "%s/%s" work-done-str effort-str))
;;             (format "%s" work-done-str))))
;; ;;;#pomodoro
;; (use-package org-pomodoro
;;     :after org-agenda
;;     :custom
;;     (org-pomodoro-ask-upon-killing t)
;;     (org-pomodoro-format "%s")
;;     (org-pomodoro-short-break-format "%s")
;;     (org-pomodoro-long-brea-format  "%s")
;;     :custom-face
;;     (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
;;     (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))
;; ;;    :hook
;;     ;; (org-pomodoro-started . (lambda () (notifications-notify
;;     ;;                                            :title "org-pomodoro"
;;     ;;                        :body "Let's focus for 25 minutes!"
;;     ;; 			   ;;                           :app-icon "~/.emacs.d/img/001-food-and-restaurant.png"
;;     ;; 			   )))
;;     ;; (org-pomodoro-finished . (lambda () (notifications-notify
;;     ;;                                            :title "org-pomodoro"
;;     ;;                        :body "Well done! Take a break."
;;     ;; 			   ;;                           :app-icon "~/.emacs.d/img/004-beer.png"
;;     ;; 			   )))
;;     :config
;;     :bind (:map org-agenda-mode-map
;;                 ("p" . org-pomodoro)))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(doom-themes-enable-bold t)
;;  '(doom-themes-enable-italic t)
;;  '(package-selected-packages
;;    (quote
;;     (doom-modeline use-package sound-wav package-utils org-pomodoro madhat2r-theme emojify doom-themes all-the-icons abyss-theme))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(doom-modeline-bar ((t (:background "#6272a4")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (el-get hydra leaf-keywords leaf which-key use-package sound-wav perspective package-utils org-pomodoro madhat2r-theme evil emojify doom-themes doom-modeline ddskk abyss-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
