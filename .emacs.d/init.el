;; init.el
;;前提ですが、私はinitローダーとかは使ってません。全部init.elに書いてます。で、機能毎にページを作って(C-q C-l)ます。ただ、それだけだと視認性が悪いので見出しとしてC-u C-u C-u ;で;を64個挿入して次の行にコメントで#付きのタイトルを付けてます。
;;こんな感じのものが機能毎に書かれてます。これでC-sやM-x occurでハッシュタグのように検索することもC-vでスクロールしていって目grepすることもC-x ]で機能毎にジャンプすることもできます。


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#Commentary:

;;;; Emacsの公式ドキュメント一覧
;; - [[info:emacs#Top][Emacs]]
;; - [[info:eintr#Top][An Introduction to Programming in Emacs Lisp]]
;; - [[info:elisp#Top][Emacs Lisp]]
;; - Reference Cards (/etc/refcards)
;;   - refcard.pdf :: Emacs
;;   - calccard.pdf :: Calc
;;   - dired-ref.pdf :: Dired
;;   - gnus-booklet.pdf :: Gnus
;;   - gnus-refcard.pdf :: Gnus
;;   - orgcard.pdf :: Org


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#Code:

(cd "~/") ; ホームディレクトリにcd


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#packeges
;;;; 初期化
;;   Emacsは init.el 読み込み後に各パッケージへのload-path設定を行い
;;   XXX-autoloads.el を読み込む。このままでは init の段階では
;;   require/locate-library ができないため、(package-initialize) を事前
;;   に実行する。

(package-initialize)
(setq package-enable-at-startup nil) ; 初期化済みなので自動初期化は停止。

;; パッケージの情報は、~/.emacs.d/elpa/archives/ に格納される。自分で
;; パッケージを作る場合は、 package-x.el の、
;; `package-upload-{buffer,file}' を利用する。archive-contents ファイ
;; ルが自動生成される。

;; パッケージ読み込み後は、読み込みライブラリを表示する。
;; （繁雑な XXXX-autoload は表示させない。）
;; ただし、package-menu-execute 時のみ、(XXX-autoload.elを) 表示させない。
(defadvice package-menu-execute (around tkw-package-menu-execute-suppress-load-messages)
  "Suppress displaying load file messages."
  (let ((force-load-messages nil))
    ad-do-it))
(ad-activate 'package-menu-execute)

;;;; Package Archives
(set-variable 'package-archives
              '(("gnu" . "http://elpa.gnu.org/packages/")
                ("melpa" . "http://melpa.milkbox.net/packages/")
                ;; sunrise-commander
                ;; ("SC"   . "http://joseito.republika.pl/sunrise-commander/")
                ;; org-mode
                ("org"   . "http://orgmode.org/elpa/")
                ))
;; ローカルレポジトリを追加
;; (when (file-exists-p "~/.emacs.d/local-packages/archive-contents")
;;   (pushnew '("local" . "~/.emacs.d/local-packages/")
;;               package-archives :test 'equal))

;;;;; Marmalade について
;;
;; MELPAとMarmalade の違い
;; | archives   | MELPA                   | Marmalade            |
;; |------------+-------------------------+----------------------|
;; | source     | Public Repository       | Upload manually      |
;; | update     | Automatic               | Manual               |
;; | XXX-pkg.el | automatically generated | prepare by oneself   |
;; | version    | year-date-revision      | prepaed by oneself   |
;; | curaton    | relatively safe         | random fork possible |
;;
;; marmalade は危険なファイルが入る可能性があるので、専用の関数で処理する。
;; 利用後は M-x init-package-archives して、もとに戻す。
;;(defun list-packages-marmalade ()
;;  (interactive)
;;  (setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")))
;;  (list-packages))

;;;; use-package
;; - url :: https://github.com/jwiegley/use-package
;; 非標準パッケージは use-package で管理する。
;; （標準ライブラリは use-package では管理しない）
;; これを利用できない環境では、パッケージ管理のソフトはインストールしない。
;;;;; 起動時の use-package の抑止
;; - init.el を外部に持ちだした時など、use-package を抑止したいときは
;;   Emacs を、オプション "--qq" で起動する。
;; - use-package が未インストールか、抑止されている場合は空マクロにする。
(eval-and-compile
(when (or (member "--qq" command-line-args)
          (null (require 'use-package nil t)))
  (warn "`use-package' is unavailable!  Please install it via `M-x list-packages' if possible.")
  (defmacro use-package (&rest _args))))
;; 後の startup.el におけるオプション認識エラーを防止
(add-to-list 'command-switch-alist '("--qq" . (lambda (switch) nil)))

;;;;; 形式的宣言
(use-package use-package :no-require t :ensure t :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#bind-key
;;; キーボード設定
;;;; bind-key

;; bind-key* は、emulation-mode-map-alists を利用することにより、
;; minor-mode よりも優先させたいキーのキーマップを定義できる。
;; bind-key.el がない場合は普通のbind-key として振る舞う。
(use-package bind-key :no-require t :defer t :ensure t)
(eval-and-compile
(unless (require 'bind-key nil t)
  (defun bind-key (key cmd &optional keymap)
    (define-key (or keymap global-map) (kbd key) cmd))
  (defun bind-key* (key cmd) (global-set-key (kbd key) cmd))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#appearance

 ;; font
 (when (member "Noto Sans CJK JP Regular" (font-family-list))
 (add-to-list 'default-frame-alist '(font . "Noto Sans CJK JP Regular 14")))
 ;;ダークモード
 (load-theme 'tango-dark t)




 ;; フォント設定(WSL用)
;; デフォルト フォント
(set-face-attribute 'default nil :family "Migu 1M" :height 120)
;; プロポーショナル フォント
(set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 120)
;; 等幅フォント
(set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 120)
;; ツールチップ表示フォント
(set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)

;;;; (emacs) 14 Controlling the Display 
;;;;; (emacs) 14.16 Useless Whitespace
;; 行末の空白を表示
(setq-default show-trailing-whitespace nil)
(defun turn-on-show-trailing-whitespace  () (interactive) (setq show-trailing-whitespace t))
(defun turn-off-show-trailing-whitespace () (interactive) (setq show-trailing-whitespace nil))
(defun toggle-show-trailing-whitespace () (interactive) (callf null show-trailing-whitespace))
(add-hook 'prog-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'org-mode-hook 'turn-on-show-trailing-whitespace)

;;;;; (emacs) 21.15 Tool Bars
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; line-number display
(line-number-mode t)
(column-number-mode t)

;; #time.el
(setq display-time-day-and-date t)
(set-variable 'display-time-24hr-format t)
(set-variable 'display-time-string-forms
              '(month "/" day
                      " " 24-hours ":" minutes ;; ":" seconds
                      (if mail " Mail" "")))
(display-time)

;;;;; (emacs) 14.20 Displaying the Cursor
(blink-cursor-mode 0)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#control-appearance
;; 文字の大きさを一時的に変更するには text-scale-adjust
;; デフォルトでは C-x C-0
;; + 連打→拡大
;; - 連打→縮小
;; 0 → 元に戻す

;;;;; (emacs) 14.21 Line Truncation
(bind-key "C-c t" 'toggle-truncate-lines)

;; ;;;;; (emacs) 14.22 Visual Line Mode
;; ;;(global-visual-line-mode t)
;; tukawa nai. kirikae takereba, M-x global-visual-line-mode de.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#navigation #movecursor
;;;;; (emacs) 7.2 Changing the Location of Point
(bind-key "C-x :" 'goto-line) ; M-g g に加えて追加

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#region
;;;; (emacs) 11.4 The Mark Ring
;; C-u C-SPC C-SPC... で繰返しマークをpopできるようにする.
(setq set-mark-command-repeat-pop t)
;;;; (emacs) 11.7 Disabling Transient Mark Mode
(transient-mark-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#cut-and-paste
;;;; (emacs) 12 Killing and Moving Text
;;;;; (emacs) 12.1.2 Killing by Lines
(setq kill-whole-line t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#search
;;;; (emacs) 15.1 Incremental Search
(setq lazy-highlight-initial-delay 0) ; isearch のハイライト反応を良くする。
(bind-key "C-k" 'isearch-edit-string isearch-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#language
;;;; (emacs) 22 International
;;;;; (emacs) 22.2 Language Environments
;;coding関係の設定の前にこれを行う。
(set-language-environment "Japanese")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#coding-system
;;;;; (emacs) 22.6 Recognizing Coding Systems
(prefer-coding-system
 (cond ((equal system-type 'windows-nt) 'utf-8-dos)
       (t 'utf-8)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#comment
;;;; (emacs) 26.5 Comments
(bind-key "C-c ;" 'comment-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#package
;; ;;;;; emacs-lisp/package-x.el
;; ;; package-upload-buffer は tarファイルでも指定可能。
;; ;; 自作ツールはパッケージ化すると、autoloadなどが自動化される。

;; ;; +-----------------+  tar command で生成      package-upload-file
;; ;; | hogehoge.el     |  +------------------+   +--------------------------+
;; ;; | hogehoge-pkg.el +->| hogehoge-VER.tar +-->| package-base-directory/  |
;; ;; | 関連ファイル    |  +---------+--------+   |   hogehoge-VER.tar       |
;; ;; | README (descr.) |            |            +------------+-------------+
;; ;; +-----------------+            |         package-install | autoload 生成
;; ;;                                |                         v バイトコンパイル
;; ;;                                |            +--------------------------+
;; ;;                                |            | package-base-directory/  |
;; ;;                                +----------->|   hogehoge-autoloades.el |
;; ;;                      package-install-file   |   hogehoge.elc           |
;; ;;                                             |   misc files...          |
;; ;;                                             +--------------------------+
;; ;(use-package package-x
;; ;  :commands (package-upload-file package-upload-buffer)
;; ;  :config
;; ;  (setq package-archive-upload-base (locate-user-emacs-file "local-packages")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#under-construction

;; ;;;; (emacs) 50.1 If <DEL> Fails to Delete
;; (normal-erase-is-backspace-mode 1)

;; ;;;; 20 Minibuffers
;; ;;;;; 20.6.1 Basic Completion Functions
;; ;; completion の際に 大文字・小文字の違いは無視する。
;; (setq completion-ignore-case t)

;; ;;;;; 20.7 Yes-or-No Queries
;; (fset 'yes-or-no-p 'y-or-n-p)

;; ;;;;; 20.13 Recursive Minibuffers
;; ;; ミニバッファ内で再帰的編集を許す
;; (setq enable-recursive-minibuffers t)

;; ;;;; 21 Command Loop
;; ;; キー入力の処理順番
;; ;; イベント入力 (read-event)
;; ;; → キー入力 (read-key-sequence)
;; ;; Translation of Input Events [[info:elisp#Event Mod]]
;; ;; - keyboard-translate-table
;; ;; - input-method-function
;; ;; Translation of Keyboard Input [[info:elisp#Translation Keymaps]]
;; ;; - input-decode-map
;; ;; - local-function-key-map (parent: function-key-map)
;; ;; - key-translation-map (iso-transl.el)
;; ;; Searching the Active Keymaps :: [[info:elisp#Searching Keymaps]]
;; ;;   キー入力を、コマンドに変換する
;; ;; - overriding-terminal-local-map :: 滅多に使われない。
;; ;; - overriding-local-map :: これより下位のkeymapを全て無効にするので実用性はない。
;; ;; - (keymap char property at point)
;; ;; - emulation-mode-map-alists :: 最優先キーにはこれを利用する
;; ;; - minor-mode-overriding-map-alist :: Major Mode が Minor Mode をオーバーライドするのに使用。
;; ;; - minor-mode-map-alist
;; ;; - (Keymap text property at point)
;; ;; - (current local-map) :: Major Mode
;; ;; - current global-map :: Global Keymap

;; ;; Altキーの代替として、M-a をprefixとして使用する。
;; ;; 旧 M-a (backward-sentence は M-A に割り当てる)
;; (bind-key "M-a" 'event-apply-alt-modifier function-key-map)
;; (bind-key "M-A" 'backward-sentence)

;; ;;;;; 21.8.3 Modifying and Translating Input Events
;; ;; C-h を DEL にして、C-z を C-h にする。
;; (keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
;; (keyboard-translate ?\C-z ?\C-h)  ; translate `C-z' to `C-h'.

;; ;;;; 23 Major and Minor Modes
;; ;; *scratch* は text-mode にするが、一旦、lisp-interactive-mode になったらそちらをデフォルトにする。
;; (setq initial-major-mode 'fundamental-mode)
;; ;;(add-hook 'lisp-interaction-mode-hook
;; ;;          (lambda () (setq initial-major-mode 'lisp-interaction-mode)))

;; ;;;;; 23.6 Font Lock Mode
;; ;; * font-lock-defaults の書式
;; ;;   (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD
;; ;;           [SYNTAX-ALIST [SYNTAX-BEGIN OTHER-VARS…]]]])
;; ;;   KEYWORDS ::= SYMBOL(font-lock-keywords) | SYMBOL(function) | LIST (font-lock-keywords)

;; ;;;;; 23.7 Automatic Indentation of Code
;; ;; インデントエンジン
;; ;; - 参照 :: emacs-lisp/smie.el
;; ;; - 参考 :: progmodes/modula2, progmodes/octave-mod, progmodes/prolog,
;; ;;           progmodes/ruby-mode, progmodes/sh-script, textmodes/css-mode,
;; ;;           tuareg.el
;; ;;           https://github.com/deactivated/sql-smie-mode/blob/master/sql-smie-mode.el
;; ;; * 基本構成
;; ;;   文法解析・字句解析・インデント計算関数（引数：字句と文脈）
;; ;;   (smie-setup XYZ-smie-grammar #'XYZ-smie-rules
;; ;;               :forward-token   #'XYZ-smie--forward-token
;; ;;               :backward-token  #'XYZ-smie--backward-token)
;; ;; |------------------------+----------------------------|
;; ;; | 設定パラメータ         | デフォルト値               |
;; ;; |------------------------+----------------------------|
;; ;; | forward-sexp-function  | smie-forward-sexp-command  |
;; ;; | backward-sexp-function | smie-backward-sexp-command |
;; ;; |------------------------+----------------------------|
;; ;; * 演算子順位構文解析（ドラゴンブック4.6参照）
;; ;;
;; ;;        smie-bnf->prec2            smie-prec2->grammar
;; ;;   BNF (BNF文法) ----> Prec2 ---+---> Prec2 ----> Grammar
;; ;;                                |
;; ;;                                | smie-merge-prec2s
;; ;;                                |
;; ;;   Prec (順位) ------> Prec2 ---+
;; ;;         smie-precs->prec2
;; ;;
;; ;; ** 文法からの優先順位の決定方法
;; ;;    a → bXc
;; ;;    b → dYe の場合は、X⋗Y が自動的に決まる。
;; ;;    または
;; ;;    a -> bXc or dYe として、別に X⋗Y を指定する。
;; ;;    ※ Precは、smie-bnf->prec2の第二以降の引数としても指定可能。
;; ;;      その場合は、必要な優先順位を１引数に入れると、複数の引数間の優先順は
;; ;;      SMIE が自動的に決定する。
;; ;;      e.g. a=b+c*d^e;f=g-h/i の場合、`;' ⋗ `=' ⋗ `^' ⋗ `*' ≐ `/' ⋗ `+' ≐ `-' の優先度。
;; ;; - 終端記号は opener / closer / neither になり、closer に対しては最外殻を除き、
;; ;;   (:before . opener) がコールバック関数に渡され、それでインデント量を計算する。
;; ;; - syntax-tableのコメントや括弧は自動的に文法として認識される。
;; ;; - :list-intro がきた場合は、arg の後ろに　expression のリストがくる場合は t を返す。

;; ;;;; 25 Files
;; (setq delete-by-moving-to-trash nil) ; <undocumented>
;; (defun tkw-toggle-delete-by-moving-to-trash ()
;;   "Toggle 'delete-by-moving-to-trash'."
;;   (interactive)
;;   (message "delete-by-moving-to-trash=%s"
;;            (setq delete-by-moving-to-trash
;;                  (null delete-by-moving-to-trash))))

;; ;;;; 26 Backups and Auto-Saving
;; (setq auto-save-timeout 30
;;       auto-save-interval 500)

;; ;;;; 27 Buffers
;; (bind-key "M-l" 'bury-buffer)

;; ;;;; 28 Windows
;; ;;;;; 28.11 Switching to a Buffer in a Window
;; (bind-key "C-x M-B" 'pop-to-buffer)

;; ;;;;; 28.14 Additional Options for Displaying Buffers
;; (setq split-window-preferred-function 'split-window-sensibly)
;; (setq split-height-threshold 80)
;; (setq split-width-threshold 160)

;; ;;;;; 28.20 Textual Scrolling
;; (setq next-screen-context-lines 3
;;       scroll-preserve-screen-position t
;;       scroll-conservatively most-positive-fixnum ; 4
;;       scroll-margin 4
;;       scroll-step 1)
;; (setq scroll-up-aggressively 0.0
;;       scroll-down-aggressively 0.0)

;; ;;;;; 28.9 Cyclic Ordering of Windows
;; (defun tkw-other-window ()
;;   "ウィンドウが1つしかない場合は、過去のウィンドウ配置に戻るか、左
;; 右・上下のいずれかに分割する。"
;;   (interactive)
;;   (when (one-window-p)
;;     (if (functionp 'winhist-backward) (call-interactively 'winhist-backward)
;;       (if (< (window-width) 140)
;;           (split-window-vertically)
;;         (split-window-horizontally))))
;;   (other-window 1))

;; ;; "M-o" → "M-O" へ入れ替え。
;; ;; (bind-key "M-O" (lookup-key global-map (kbd "M-o")))
;; (bind-key* "M-o" 'tkw-other-window) ; ggtags.el の navigation/facemenu-keymap と衝突。
;; (bind-key "M-O" 'facemenu-keymap)

;; ;;;;; 33.7 Character Sets
;; (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
;;           'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)

;; ;;;;; 33.10.5 Default Coding Systems
;; (setq default-process-coding-system
;;       (case system-type ('windows-nt '(cp932 . cp932))
;;                         ('darwin (require 'ucs-normalize) '(utf-8-hfs . utf-8))
;;                         (t '(undecided . utf-8))))
;; (setq selection-coding-system
;;       (case system-type ('windows-nt 'cp932)
;;                         ('darwin 'utf-8)
;;                         (t nil)))
;; ;; decode-translation-table の設定

;; ;;;;; 33.10.6 Specifying a Coding System for One Operation
;; (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;; ;;;;; 33.12 Locales
;; (setq system-time-locale "C")

;; ;;;;; 38.11 Line Height
;; (setq-default line-spacing 1)           ; 行間を2ピクセルあける

;; ;;;;; 38.12 Faces
;; ;;;;; 38.12.11 Fontsets
;; ;; フォントセットの設定
;; ;; SPECS = ((target . fonts) (target . fonts) ...)
;; ;; TARGET = t ← default for all, nil ← all
;; ;; 最初から最後に向かってつなげていく。
;; ;; TARGETのスクリプト名は、international/characters.el を参照。
;; ;; fonts の最初の要素のフォントを利用する。

;; ;;;;; 38.12.12 Low-Level Font Representation

;; ;; FAMILY-NAME
;; ;; OPENED-NAME ... XLFD Name
;; ;; FULL-NAME
;; ;;                                     +-------+
;; ;;  font-info                          |bufffer|
;; ;;  font-family-list                   +---+---+
;; ;;      |                           font-at|    composition-get-gstring
;; ;;      v                                  v    font-shape-gstring
;; ;; +---------+     +-----------+      +-----------+      +-------+
;; ;; |font-spec+---->|font-entity+----->|font-object+----->|GSTRING|
;; ;; +---------+     +-----------+      +----+------+      +-------+
;; ;;         find-font           open-font   |     font-get-glyphs
;; ;;         list-fonts                      |             +------+
;; ;;                                         +------------>|glyphs|
;; ;;                                         |             +------+
;; ;;                                         |             +---------+
;; ;;                                         +------------>+font-info|
;; ;;                                         | query-font  +---------+
;; ;;                                         |             +----------+
;; ;;                                         +------------>|variations|
;; ;;                                 font-variation-glyphs +----------+

;; ;; - font-spec parameters
;; ;; |---------+------------------------------------------------------------|
;; ;; | :family |                                                            |
;; ;; | :width  | `ultra-condensed', `extra-condensed', `condensed',         |
;; ;; |         | `semi-condensed', `normal', `semi-expanded', `expanded',   |
;; ;; |         | `extra-expanded', or `ultra-expanded'                      |
;; ;; | :weight | `ultra-bold', `extra-bold', `bold', `semi-bold', `normal', |
;; ;; |         | `semi-light', `light', `extra-light', `ultra-light'        |
;; ;; | :slant  | `italic', `oblique', `normal', `reverse-italic',           |
;; ;; |         | `reverse-oblique'.                                         |
;; ;; |---------+------------------------------------------------------------|

;; ;; - Functions with font-spec/entity/object as arguments

;; ;;   fontp/font-info/font-get/font-face-attributes/font-put/font-xlfd-name
;; ;;   font-match-p

;; ;; - For debugging

;; ;;   font-drive-otf/font-otf-alternates/draw-string

;; ;; - Font Shaping by font Backend

;; ;;   font-shape-gstring

;; ;; - Examples

;; ;;   (font-info (font-spec :family "Hiragino Kaku Gothic ProN"))
;; ;;   (setq entity (find-font (font-spec :family "Hiragino Kaku Gothic ProN")))
;; ;;   (setq object (open-font entity))
;; ;;   (font-face-attributes object)
;; ;;   (font-get-glyphs object 0 6 "漢字と日本語")
;; ;;   (font-otf-alternates object ?漢 'aalt)
;; ;;   (font-variation-glyphs object ?漢)
;; ;;   (font-shape-gstring)
;; ;;   (font-get-glyphs object 0 3 [?a ?漢 #x800])

;; ;;;;; 38.13 Fringes
;; ;; 38.13.2 Fringe Indicators
;; ;; 空行をフリンジに表示
;; (setq-default indicate-empty-lines t)

;; ;;;;; 38.21 Beeping
;; (setq visible-bell t)
;; ;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
;; ;;(setq ring-bell-function 'ignore)

;; ;;; 標準ライブラリ
;; ;; loadup.el に記述され、デフォルトでEmacsに読み込まれる
;; ;; ライブラリに関する設定は、上の「標準設定」で行なう。

;; ;;;; dired.el
;; (defvar dired-mode-map)
;; (defvar dired-actual-switches)
;; (with-eval-after-load 'dired
;;   (require 'dired-x)
;;   (add-hook 'dired-mode-hook (lambda () (setenv "LANG" "C")))
;;   ;; diredバッファの自動更新
;;   (set-variable 'dired-auto-revert-buffer t)
;;   ;; diredのサイズ表示に Kbyte, Mbyte 等の単位を使う。
;;   ;; -h :: Kbyte, Mbyte 単位の表示
;;   (set-variable 'dired-listing-switches "-alh")
;;   ;; Diredでのコピー先ディレクトリの自動推定（２窓ファイラ的動作）
;;   (set-variable 'dired-dwim-target t)
;;   ;; 再帰的にコピー・削除
;;   (set-variable 'dired-recursive-copies 'always)
;;   (set-variable 'dired-recursive-deletes 'always)
;;   ;; Drag&Drop
;;   ;; (setq dired-dnd-protocol-alist nil)
;;   ;; GNU ls は、Mac では、
;;   ;; - sudo port install coreutils (gls)
;;   ;; - sudo port install coreutils +with_default_names (ls)
;;   ;; でインストール可能
;;   (when (executable-find "gls")
;;     (setq insert-directory-program "gls"))
;;   ;; dired の sort を拡張する。
;;   ;; sorter.el のバグ修正・整理版。
;;   (defvar dired-sort-order '("" "t" "S" "X")
;;     "-t (時間) -X (拡張子) -S (サイズ) なし (アルファベット順) を切り替える。")
;;   (defvar dired-sort-order-position 0)
;;   (declare-function dired-sort-other "dired")
;;   (defun dired-rotate-sort ()
;;     "Rotate dired toggle sorting order by `dired-sort-order'"
;;     (interactive)
;;     (setq dired-sort-order-position
;;           (% (1+ dired-sort-order-position) (length dired-sort-order)))
;;     (setq dired-actual-switches
;;           (concat dired-listing-switches (elt dired-sort-order
;;                                               dired-sort-order-position)))
;;     (dired-sort-other dired-actual-switches))
;;   (bind-key "s" 'dired-rotate-sort dired-mode-map)
;;   ;; diredバッファでC-sした時にファイル名だけにマッチするように
;;   (setq dired-isearch-filenames t)
;;   )




;; ;; dired のバッファが氾濫しないように，ディレクトリを移動するだけなら
;; ;; バッファを作らないようにする．
;; ;;(defvar tkw-dired-before-buffer nil)
;; ;;(defadvice dired-advertised-find-file
;; ;;  (before kill-dired-buffer activate)
;; ;;  (setq tkw-dired-before-buffer (current-buffer)))
;; ;;(defadvice dired-advertised-find-file
;; ;;  (after kill-dired-buffer-after activate)
;; ;;  (when
;; ;;      (and
;; ;;       (eq major-mode 'dired-mode)
;; ;;       (not (string= (buffer-name (current-buffer))
;; ;;                     (buffer-name tkw-dired-before-buffer))))
;; ;;    (kill-buffer tkw-dired-before-buffer)))
;; ;;(defadvice dired-up-directory
;; ;;  (before kill-up-dired-buffer activate)
;; ;;  (setq tkw-dired-before-buffer (current-buffer)))
;; ;;(defadvice dired-up-directory
;; ;;  (after kill-up-dired-buffer-after activate)
;; ;;  (when
;; ;;      (and
;; ;;       (eq major-mode 'dired-mode)
;; ;;       (not (string= (buffer-name (current-buffer))
;; ;;                     (buffer-name tkw-dired-before-buffer))))
;; ;;    ;;(not (string-match "^[a-z]+:[/]$" (buffer-name tkw-dired-before-buffer))))
;; ;;    (kill-buffer tkw-dired-before-buffer)))

;; ;; Cygwin 環境では、diredのファイル名はutf-8のため、fopenと整合しない。
;; ;;(when (file-executable-p "c:/cygwin/bin/ls.exe")
;; ;;  (setq ls-lisp-use-insert-directory-program t)
;; ;;  (setq insert-directory-program "c:/cygwin/bin/ls.exe"))


;; ;;; Package setup

;; ;;;; ddskk.el
;; (global-set-key (kbd "C-x j") 'skk-mode)
;; ;; to avoid conflict of dired-jump in dired-x.el

;; ;;;; end of init.el
;; (message "End of loading init.el.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;#working
;; ;;;;ここからは、自分で作った混ぜこぜのinit.el



;; ;; 警告音の代わりに画面フラッシュ

;; ;;; recentf.elの設定
;; (defmacro with-suppressed-message (&rest body)
;;   "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
;;   (declare (indent 0))
;;   (let ((message-log-max nil))
;;     `(with-temp-message (or (current-message) "") ,@body)))

;; (require 'recentf)
;; (setq recentf-save-file "~/.emacs.d/.recentf")
;; (setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
;; (setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
;; ;;(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
;; (run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
;;    (with-suppressed-message (recentf-save-list))))
;; (require 'recentf-ext)

;; ;; キーバインド
;; (global-set-key (kbd "C-c o") 'recentf-open-files)


;; ;; OSのクリップボードもkill-ringに保存
;; (setq save-interprogram-paste-before-kill t)


;; ;; dired関連の設定
;; ;; open current-directory
;; (defun find-file-current-dir ()
;;   "Find-file current directory"
;;   (interactive)
;;   (find-file default-directory))
;; ;; benri kansu
;;    (defun kill-current-buffer-and/or-dired-open-file ()
;;     "In Dired, dired-open-file for a file. For a directory, dired-find-file and
;; kill previously selected buffer."
;;     (interactive)
;;     (if (file-directory-p (dired-get-file-for-visit))
;;         (dired-find-alternate-file)
;;       (dired-open-file)))

;;   (defun kill-current-buffer-and-dired-up-directory (&optional other-window)
;;     "In Dired, dired-up-directory and kill previously selected buffer."
;;     (interactive "P")
;;     (let ((b (current-buffer)))
;;       (dired-up-directory other-window)
;;       (kill-buffer b)))

;;   (defun dired-open-file-other-window ()
;;     "In Dired, open file on other-window and select previously selected buffer."
;;     (interactive)
;;     (let ((cur-buf (current-buffer)) (tgt-buf (dired-open-file)))
;;       (switch-to-buffer cur-buf)
;;       (when tgt-buf
;;         (with-selected-window (next-window)
;;           (switch-to-buffer tgt-buf)))))

;;   (defun dired-up-directory-other-window ()
;;     "In Dired, dired-up-directory on other-window"
;;     (interactive)
;;     (dired-up-directory t))

;; ;; hide details at startup
;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; ;; sort by update time at startup
;; (setq dired-listing-switches "-alth")

;; ;; change key-bind of dired to vim-like
;; (with-eval-after-load 'dired
;;  (bind-keys :map dired-mode-map
;;           ("j" . dired-next-line)
;;           ("k" . dired-previous-line)
;;           ("h" . kill-current-buffer-and-dired-up-directory)
;;           ("l" . kill-current-buffer-and/or-dired-open-file)
;;           ("f" . kill-current-buffer-and/or-dired-open-file)
;;           ("H" . dired-up-directory-other-window)
;;           ("L" . dired-open-file-other-window)))

;; (bind-key "M-." 'find-file-current-dir)

;; ;; r key to wdired
;; (require 'wdired)
;; (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; ;; ファイルをWindowsの関連付けで開く
;; (add-hook 'dired-load-hook (function (lambda ()
;;     (define-key dired-mode-map "w" 'dired-open-file)
;; )))
;; (defun dired-open-file ()
;;   "In dired, open the file named on this line."
;;   (interactive)
;;     (message "WindowsOpening %s..." (dired-get-filename))
;;     (w32-shell-execute "open" (dired-get-filename))
;;     (message "WindowsOpening %s done" (dired-get-filename))
;;    )
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-archives
;;    (quote
;;     (("gnu" . "https://elpa.gnu.org/packages/")
;;      ("melpa" . "http://melpa.org/packages/"))))
;;  '(package-selected-packages
;;    (quote
;;     (dired-open dired-launch bind-key package-utils dired-toggle ddskk org))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;; ;; 以下、Qiitaより「オススメ設定」
;; ;;[個人メモ] emacsのinit.el
;; ;;https://qiita.com/aa_dev/items/0e305d115bf49da62d77
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; https://dev.classmethod.jp/etc/emacs-setup-and-org-mode/
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (create-fontset-from-ascii-font
;; ;;  "Menlo-14:weight=normal:slant=normal"
;; ;;  nil
;; ;;  "menlokakugo")

;; ;; (set-fontset-font
;; ;;  "fontset-menlokakugo"
;; ;;  'unicode
;; ;;  (font-spec :family "Hiragino Kaku Gothic ProN")
;; ;;  nil
;; ;;  'append)

;; ;; (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))


;; ;; CUAモード(C-RET) 有効
;; (cua-mode t)
;; (setq cua-enable-cua-keys nil)

;; ;; C-h > バックスペース★バック
;; (global-set-key (kbd "C-h") 'delete-backward-char)

;; ;; デフォルトエンコーディングをUTF-8 に
;; (prefer-coding-system 'utf-8)

;; ;; 自動保存されるバックアップファイルの置き場所を ~/.emacs.d/backup に変更する
;; (setq backup-directory-alist
;;   (cons (cons ".*" (expand-file-name "~/.emacs.d/backup")) 
;;         backup-directory-alist)) 
;; (setq auto-save-file-name-transforms 
;;       `((".*", (expand-file-name "~/.emacs.d/backup/") t))) 

;; ;; ツールバー非表示
;; (tool-bar-mode -1)

;; ;; 時刻をモードラインに表示
;; (display-time-mode t)

;; ;; 行番号をモードラインに表示
;; (column-number-mode t)

;; ;; 左端に行数を表示させる
;; (global-linum-mode t) 

;; ;; 対応する括弧をハイライト
;; ;(show-paren-mode 1)

;; ;; 起動時の Emacsロゴなどのメッセージを出さない
;; (setq inhibit-startup-message t) 

;; ;; *scratch* バッファの初期メッセージを消す
;; (setq initial-scratch-message "")

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; https://qiita.com/blue0513/items/ff8b5822701aeb2e9aae
;; ;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; 直前のバッファに戻る
;; (global-set-key (kbd "s-[") 'switch-to-prev-buffer)

;; ;; 次のバッファに進む
;; (global-set-key (kbd "s-]") 'switch-to-next-buffer)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; https://qiita.com/yn01/items/b8d3dcb5be9078a6e27f
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; 環境を日本語、UTF-8にする
;; (set-locale-environment nil)
;; (set-language-environment "Japanese")
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (prefer-coding-system 'utf-8)

;; ;; スタートアップメッセージを表示させない
;; ;(setq inhibit-startup-message t)

;; ;; バックアップファイルを作成させない
;; ; (setq make-backup-files nil)

;; ;; 終了時にオートセーブファイルを削除する
;; ; (setq delete-auto-save-files t)

;; ;; タブにスペースを使用する
;; (setq-default tab-width 4 indent-tabs-mode nil)

;; ;; 改行コードを表示する
;; (setq eol-mnemonic-dos "(CRLF)")
;; (setq eol-mnemonic-mac "(CR)")
;; (setq eol-mnemonic-unix "(LF)")

;; ;; 複数ウィンドウを禁止する
;; (setq ns-pop-up-frames nil)

;; ;; ウィンドウを透明にする
;; ;; アクティブウィンドウ／非アクティブウィンドウ（alphaの値で透明度を指定）
;; (add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

;; ;; メニューバーを消す
;; ;(menu-bar-mode -1)

;; ;; ツールバーを消す
;; (tool-bar-mode -1)

;; ;; 列数を表示する
;; (column-number-mode t)

;; ;; 行数を表示する
;; (global-linum-mode t)

;; ;; カーソルの点滅をやめる
;; (blink-cursor-mode 0)

;; ;; カーソル行をハイライトする
;; (global-hl-line-mode t)

;; ;; 対応する括弧を光らせる
;; (show-paren-mode 1)

;; ;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
;; ;(setq show-paren-style 'mixed)
;; ;(set-face-background 'show-paren-match-face "grey")
;; ;(set-face-foreground 'show-paren-match-face "black")

;; ;; スペース、タブなどを可視化する
;; ;(global-whitespace-mode 1)

;; ;; スクロールは１行ごとに
;; (setq scroll-conservatively 1)

;; ;; シフト＋矢印で範囲選択
;; (setq pc-select-selection-keys-only t)
;; ;(pc-selection-mode 1)

;; ;; C-kで行全体を削除する
;; ;(setq kill-whole-line t)

;; ;;; dired設定
;; (require 'dired-x)

;; ;; "yes or no" の選択を "y or n" にする
;; (fset 'yes-or-no-p 'y-or-n-p)

;; ;; beep音を消す
;; (defun my-bell-function ()
;;   (unless (memq this-command
;;         '(isearch-abort abort-recursive-edit exit-minibuffer
;;               keyboard-quit mwheel-scroll down up next-line previous-line
;;               backward-char forward-char))
;;     (ding)))
;; (setq ring-bell-function 'my-bell-function)

;; ;; Macのキーバインドを使う
;; ;(mac-key-mode 1)

;; ;; Macのoptionをメタキーにする
;; ;(setq mac-option-modifier 'meta)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;https://qiita.com/melito/items/34fa31d2c96d187980e7
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; フレームサイズの指定
;; (set-frame-size (selected-frame) 86 40)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Org mode
;; ;; http://www.mhatta.org/wp/2018/08/16/org-mode-101-1/
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ; ファイルの場所
;; (setq org-directory "~/ownCloud/Org")
;; ;(setq org-directory "~/Dropbox/Org")
;; (setq org-default-notes-file "notes.org")

;; ; Org-captureの設定

;; ; Org-captureを呼び出すキーシーケンス
;; (define-key global-map "\C-cc" 'org-capture)
;; ; Org-captureのテンプレート（メニュー）の設定
;; (setq org-capture-templates
;;       '(("n" "Note" entry (file+headline "~/ownCloud/Org/notes.org" "Notes")
;;          "* %?\nEntered on %U\n %i\n %a")
;;         ))

;; ; メモをC-M-^一発で見るための設定
;; ; https://qiita.com/takaxp/items/0b717ad1d0488b74429d から拝借
;; (defun show-org-buffer (file)
;;   "Show an org-file FILE on the current buffer."
;;   (interactive)
;;   (if (get-buffer file)
;;       (let ((buffer (get-buffer file)))
;;         (switch-to-buffer buffer)
;;         (message "%s" file))
;;     (find-file (concat "~/ownCloud/Org/" file))))
;; (global-set-key (kbd "C-M-^") '(lambda () (interactive)
;;                                  (show-org-buffer "notes.org")))

;; ;;
;; (setq desktop-save-mode t)
;; (setq desktop-globals-to-save kill-ring)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages (quote (use-package recentf-ext bind-key))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; ;;;; (emacs) 8 Minibuffers
;; ;; ミニバッファの変化が激しいと思うときは、'grow-onlyに。
;; (setq resize-mini-windows 'grow-only)

;; ;;; 標準設定
;; ;;;; (emacs) 7 Basic
;; ;;;;; (emacs) 7.1 Inserting Text
;; ;; `C-q 数字' で文字を入力する際の進数
;; (setq read-quoted-char-radix 16)


;; ;;;;; (emacs) 12.3.2 Cut and Paste with Other Window Applications
;; ;; VNC等で動きが遅くならないための工夫
;; (setq select-active-regions nil)

;; ;;;;; (emacs) 14.18 Optional Mode Line Features
;; (setq line-number-display-limit 10000000)
;; (setq line-number-display-limit-width 1000)

;; ;; フレームの横幅が一定以下になれば自動的に truncate-window-mode にする。
;; ;; nil の場合はこの機能を無効にする。（デフォルトは50）

;; ;;;;; (emacs) 14.22 Visual Line Mode
;; ;;(global-visual-line-mode t)

;; ;;;; (emacs) 18.2 Visiting Files
;; (when (require 'openwith nil t)
;;   ;; openwith-file-handler でデバッガに落ちないようにする。
;;   (defadvice openwith-file-handler (around tkw-openwith-handler activate)
;;     (let (debug-on-error)
;;       ad-do-it))
;;   ;; ディレクトリを開こうとする場合は、OSのファイルブラウザで開けるようにする。
;;   (defadvice find-file (around tkw-find-file activate)
;;     "Open directory by external application if its name ends with /."
;;     (if (and (file-directory-p (ad-get-arg 0))
;;              (string-match "/$" (ad-get-arg 0))
;;              (y-or-n-p (concat "Open " (ad-get-arg 0)
;;                                " in File Browser? ")))
;;         ;; openwith は、強制エラーを発生させるので、 debug-on-error を
;;         ;; nil にして、デバッガに入るのを抑止する。
;;         (cond ((eq system-type 'windows-nt)
;;                (openwith-open-windows (list (ad-get-arg 0))))
;;               ((eq system-type 'gnu/linux)
;;                (openwith-open-unix "nautilus" (list (ad-get-arg 0))))
;;               (t
;;                (openwith-open-unix "open" (list (ad-get-arg 0)))))
;;       ad-do-it)))
;; ;; warn only if file size is more than 100Mbyte.
;; (setq large-file-warning-threshold 100000000)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (ddskk-posframe use-package recentf-ext pallet init-loader evil dired-open ddskk))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; ;;;; (emacs) 20 Windows
;; (bind-key "A-M-n" 'enlarge-window)
;; (bind-key "A-M-p" 'shrink-window)
;; (bind-key "A-M-f" 'enlarge-window-horizontally)
;; (bind-key "A-M-b" 'shrink-window-horizontally)
;; ;;(bind-key "<C-right>" (command (scroll-left 8)))
;; ;;(bind-key "<C-left>" (command (scroll-right 8)))
;; ;;(bind-key "<M-up>" (command (scroll-up 1)))
;; ;;(bind-key "<M-down>" (command (scroll-down 1)))
;; ;;(bind-key "<C-right>" (command (scroll-left 1)))
;; ;;(bind-key "<C-left>" (command (scroll-right 1)))
;; ;; 現在のウィンドウを垂直方向に伸ばす。まず、下にウィンドウがあれば、
;; ;; それを消して、無ければ、上を消して、上もなければ、
;; ;; delete-other-windowsする。
;; (defun tkw-enlarge-window-vertically ()
;;  (command
;;    (let ((current-tl  (car (window-edges (selected-window))))
;;          (next-tl     (car (window-edges (next-window))))
;;          (previous-tl (car (window-edges (previous-window)))))
;;      (cond ((= current-tl next-tl)
;;             (other-window 1) (delete-window)
;;             (other-window -1))
;;            ((= current-tl previous-tl)
;;             (other-window -1) (delete-window))
;;            (t (delete-other-windows))))))
;; (bind-key "C-x 9" 'tkw-enlarge-window-vertically)
;; ;; 上下スクロールする際に、カーソルが追随するかどうかを切替える。

;; ;;;; (emacs) 21 Frames and Graphical Displays

;; ;;;;; (emacs) 21.7 Frame Commands
;; (bind-key "A-M-o" 'other-frame)

;; ;;;; (emacs) 25.1 Words
;; (bind-key "C-M-h" 'backward-kill-word) ; 単語をまとめてBS。

;; ;;;; (emacs) 25.8 Outline Mode

;; ;;;; (emacs) 32.7 Mail-Composition Methods
;; (setq mail-user-agent 'gnus-user-agent) ; Gnusをメールに使用する。

;; ;;;; (emacs) 36 Running shell commands from Emacs
;; ;; SHELL環境変数が使用される。
;; ;; CYGWIN 環境では、SHELL環境変数が "/bin/bash" などだと、Emacsでは解釈できない。
;; (if (equal window-system 'w32)
;;     (if (executable-find "zsh")
;;         (progn
;;           (set-variable 'shell-file-name "zsh")
;;           (set-variable 'explicit-shell-file-name "zsh"))
;;       (message "Warning! zsh not found!")))

;; ;;;; (emacs) 47.1 The Package Menu Buffer
;; ;;;;; emacs-lisp/packages.el
;; ;; (bind-key "C-c P" 'list-packages)

;; ;;;; (emacs) 48 Customization
