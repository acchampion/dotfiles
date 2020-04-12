;; Adam's .emacs file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq select-enable-clipboard t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'meta)
(scroll-bar-mode -1)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(require 'package) ;; You might already have this line
(package-initialize) ;; You might already have this line

(advice-add 'python-mode :before 'elpy-enable)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Set up "useful" coding environment.
;; Source: https://github.com/tuhdo/emacs-c-ide-demo
(defalias 'yes-or-no-p 'y-or-n-p)
;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)


;; (load-theme 'solarized-dark)

;; Automatically turn on references, math mode with AUCTeX.
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(autoload 'longlines-mode
   "longlines.el"
   "Minor mode for automatically wrapping long lines." t)

(setenv "PATH"
  (concat "~/bin" ":"
  (concat "/Library/TeX/texbin" ":"
  (concat "/usr/local/bin" ":"
(getenv "PATH")))))
(setenv "TEXMFMAIN"
	(concat "/usr/local/texlive/texmf-local" ":"
		(concat "/usr/local/texlive/2020/texmf-dist"
  (getenv "TEXMFMAIN"))))

;; Use PDF mode by default
(setq-default TeX-PDF-mode t)
;; (setq TeX-source-correlate-method 'synctex)
(setq TeX-evince-dbus-p nil)

;; Make emacs aware of multi-file projects
(setq-default TeX-master nil)
(setq scroll-step 1)

;; Turn on flyspell in every text mode.
(setq ispell-program-name "/usr/local/bin/aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
(setq ispell-list-command "--list")
(setq flyspell-issue-welcome-flag nil) ;; fix flyspell problem
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))
(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)


;; Turn on org mode.
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)

;; Turn on ps-print.
(require 'lpr)
(require 'ps-print)
(require 'enscript)
;; Add Cousine to supported printing fonts.
;;(load ~"/.emacs.d/enscript.el")
(setq ps-font-info-database
    (append
        '((Cousine
            (fonts  (normal      . "Cousine")
                    (bold        . "Cousine-Bold")
                    (italic      . "Cousine-Italic")
                    (bold-italic . "Cousine-BoldItalic"))
            (size . 10.0)
            (line-height . 12.8125)
            (space-width . 6.00098)
            (avg-char-width . 6.00098)))
        ps-font-info-database))
(setq ps-font-family 'Cousine)
(setq ps-font-size 10)
(setq ps-always-build-face-reference t)
(setq ps-header-frame-alist
   (quote
    ((fore-color . 0.0)
     (back-color . 1.0)
     (border-width . 0.4)
     (border-color . 0.0)
     (shadow-color . 0.0))))
(setq ps-spool-duplex t)
(setq ps-spool-tumble t)

;; Setup Org-mode HTML export.
(setq org-export-with-smart-quotes t)
(setq user-mail-address "champion@cse.ohio-state.edu")

;; Turn off shell mode bold text from previous commands.
(setq comint-highlight-input nil)

;; Langtool: fix spelling errors for text mode
(require 'langtool)
(setq langtool-java-tool-jar "/usr/local/Cellar/languagetool/4.9/libexec/languagetool.jar")
(setq langtool-bin "/usr/local/bin/languagetool")
(setq langtool-default-language "en-US")
(setq langtool-mother-tongue "en")
(setq sentence-end-double-space nil)

;; Programming
(setq c-default-style "linux") ; set style to "linux"
(setq c-basic-offset 4)
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'cedet)
(require 'compile)
(require 'cc-mode)
(setq c-cleanup-list
      (quote
       (brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces one-liner-defun defun-close-semi scope-operator compact-empty-funcall)))
(setq c-font-lock-extra-types
   (quote
    ("\\sw+_t" "bool" "complex" "imaginary" "FILE" "lconv" "tm" "va_list" "jmp_buf" "Lisp_Object" "&")))


;; Enable C/C++ debugging
(require 'font-lock)
(require 'semantic)
(setq compilation-ask-about-save nil          ; Just save before compiling
      compilation-always-kill t               ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error) ; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; Source: https://github.com/tuhdo/emacs-c-ide-demo
(global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'irony-mode)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

;; GDB
 ;; use gdb-many-windows by default
 ;; Display source file containing the main routine at startup
(setq gdb-many-windows t)
(setq gdb-show-main t)


;; Helm settings from https://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(helm-mode t)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(setq helm-split-window-in-side-p t ; open helm buffer inside current window, not occupy whole other window
       helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
       helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
       helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
       helm-ff-file-name-history-use-recentf t
       helm-quick-update t
       helm-candidate-number-limit 20
       helm-ff-skip-boring-files t)
(global-set-key (kbd "M-x") 'helm-M-x)
 ;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
 ;; (setq helm-buffers-fuzzy-matching t
 ;;       helm-recentf-fuzzy-match t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "ack")
   (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
         helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))
 ;; (setq helm-semantic-fuzzy-match t
 ;;     helm-imenu-fuzzy-match    t)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
 ;; (setq helm-locate-fuzzy-match t)
(global-set-key (kbd "C-c h o") 'helm-occur)
 ;; (setq helm-apropos-fuzzy-match t)
 ;; (setq helm-lisp-fuzzy-completion t)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(add-hook 'eshell-mode-hook
           #'(lambda ()
               (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; Company mode
(require 'company)
(require 'popup)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

 ;; Helm with Projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)
 ;; (helm-projectile-on)
 
 ;; Helm with Gtags
(setq  helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "C-c g"
        helm-gtags-suggested-key-mapping t
  )

(require 'helm-gtags)
 ;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
 ;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Miscellany
;; (setq gc-cons-threshold 1000000)
(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8 (http://pages.sachachua.com/.emacs.d/Sacha.html)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Smart Parens
(require 'smartparens)
(require 'smartparens-config)
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
(require 'flycheck)
(global-flycheck-mode)
(setq flycheck-chktexrc "~/.chktexrc")
(blink-cursor-mode 0)

(global-linum-mode t) ;; enable line numbers globally
(setq linum-format "%d ")

(elpy-enable)
(setq elpy-rpc-python-command "python3")

;; (require 'perl-use-utf8-coding)

(setq speedbar-show-unknown-files t)
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; Set "custom" variables so we don't depend on Custom in Emacs.
(setq blink-cursor-mode nil)
(setq column-number-mode t)
(setq enscript-after-hook
   (quote
    ((lambda nil
       (when preview-file
         (shell-command
           (concat "rm "
             (replace-regexp-in-string ".ps" ".pdf" (eval preview-file))))
         (shell-command
           (concat "ps2pdf -sFONTPATH="
             (concat
               (file-name-as-directory
                 (getenv "HOME"))
             ".fonts "
             (eval preview-file))))
         (shell-command
           (concat "open "
             (replace-regexp-in-string ".ps" ".pdf" (eval preview-file))))
       )))))
(setq inhibit-startup-screen t)
(setq line-numbers-p t)
(setq org-agenda-export-html-style nil)
(setq org-export-html-style-include-default nil)
(setq org-highlight-latex-and-related (quote (latex script entities)))
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq preview-file "emacs-enscript.ps")
(setq show-paren-mode t)
(setq size-indication-mode t)
(setq speedbar-default-position (quote left))
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-right-side nil)
(setq tool-bar-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(background-color "#082028")
 '(background-mode dark)
 '(blink-cursor-mode nil)
 '(c-cleanup-list
   (quote
    (brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces one-liner-defun defun-close-semi scope-operator compact-empty-funcall)))
 '(c-font-lock-extra-types
   (quote
    ("\\sw+_t" "bool" "complex" "imaginary" "FILE" "lconv" "tm" "va_list" "jmp_buf" "Lisp_Object" "&")))
 '(column-number-mode t)
 '(cursor-color "#657b83")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "c1afd422fab9cb7ad2422523d0491abc8f397fea76ec57bf27c70b50bfa04243" "8eafb06bf98f69bfb86f0bfcbe773b44b465d234d4b95ed7fa882c99d365ebfd" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(ede-project-directories (quote ("/Users/acc/Teaching/CSE2431/lab1")))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(foreground-color "#657b83")
 '(frame-background-mode (quote dark))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(inhibit-startup-screen t)
 '(line-numbers-p t)
 '(ns-function-modifier (quote meta))
 '(org-agenda-export-html-style nil t)
 '(org-export-html-style-include-default nil t)
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(org-html-head-include-default-style nil t)
 '(org-html-head-include-scripts t t)
 '(org-startup-folded (quote showeverything))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/"))))
 '(package-check-signature (quote allow-unsigned))
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
    (projectile company exec-path-from-shell gnu-elpa-keyring-update flycheck flymake-css flymake-json powerline dart-mode company-irony company-irony-c-headers flycheck-irony use-package smartparens function-args sr-speedbar x86-lookup helm-projectile company-shell company-web csv-mode auctex web-beautify company-math flyspell-correct-helm counsel helm-gtags theme-changer writegood-mode wc-mode swift-mode solarized-theme minimap langtool helm flymake-shell flymake-python-pyflakes ess elpy ecb color-theme-solarized color-theme)))
 '(powerline-gui-use-vcs-glyph t)
 '(preview-file "emacs-enscript.ps")
 '(ps-always-build-face-reference t)
 '(ps-header-frame-alist
   (quote
    ((fore-color . 0.0)
     (back-color . 1.0)
     (border-width . 0.4)
     (border-color . 0.0)
     (shadow-color . 0.0))))
 '(ps-spool-duplex t)
 '(ps-spool-tumble t)
 '(python-flymake-command (quote ("(\"flake8\" \"-)")))
 '(python-shell-interpreter "python3")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(solarized-broken-srgb t)
 '(solarized-distinct-fringe-background nil)
 '(solarized-high-contrast-mode-line nil)
 '(solarized-italic t)
 '(solarized-underline nil)
 '(solarized-use-variable-pitch nil)
 '(speedbar-default-position (quote left))
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-right-side nil t)
 '(tool-bar-mode nil)
 '(underlay-font "Arimo-100"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#042028" :foreground "#708183" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "JetBrains Mono"))))
 '(aquamacs-variable-width ((t (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 150 :width normal :family "Menlo"))) t)
 '(bold ((t (:weight bold))))
 '(cursor ((t (:background "#042028" :foreground "#657b83" :inverse-video t))))
 '(echo-area ((t (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :width normal :family "Menlo"))) t)
 '(error ((t (:underline (:color "#dc322f" :style wave)))))
 '(escape-glyph ((t (:foreground "#268bd2"))))
 '(fixed-pitch ((t (:family "Liberation Mono"))))
 '(fixed-pitch-serif ((t (:family "Courier"))))
 '(flycheck-fringe-error ((t (:foreground "#dc322f"))))
 '(flycheck-fringe-warning ((t (:foreground "orange3"))))
 '(font-latex-doctex-documentation-face ((t (:background "#333" :foreground "#CCC"))))
 '(font-latex-math-face ((t (:foreground "gray50"))))
 '(font-latex-script-char-face ((t (:foreground "Red3"))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#b58901" :weight bold))))
 '(font-latex-sedate-face ((t (:foreground "#6c71c4"))))
 '(font-latex-string-face ((t (:foreground "#cb4b16"))))
 '(font-latex-verbatim-face ((t (:inherit fixed-pitch :foreground "burlywood4"))))
 '(font-lock-comment-delimiter-face ((t (:inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-comment-face ((t (:inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-constant-face ((t (:foreground "#660EFF" :slant italic :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "orange3"))))
 '(font-lock-keyword-face ((t (:foreground "#666699" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#008000" :weight bold))))
 '(font-lock-type-face ((t (:foreground "#993366" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "lightblue3"))))
 '(fringe ((t (:background "#0a2832"))))
 '(glyphless-char ((t (:height 1.0))))
 '(helm-M-x-key ((t (:inherit default :foreground "#D05E00" :underline t))))
 '(helm-ff-directory ((t (:inherit default :weight bold))))
 '(helm-ff-dirs ((t (:inherit default :weight bold))))
 '(helm-ff-dotted-directory ((t (:inherit default :background "#eee8D6"))))
 '(helm-ff-dotted-symlink-directory ((t (:inherit default :background "#eee8D6" :slant oblique))))
 '(helm-ff-executable ((t (:inherit default))))
 '(helm-ff-invalid-symlink ((t (:inherit default :underline (:color "#dc322f" :style wave)))))
 '(helm-ff-prefix ((t (:inherit default :background "#EEE6D6"))))
 '(helm-ff-symlink ((t (:foreground "#D05E00" :slant italic))))
 '(helm-grep-file ((t (:foreground "#7d80cb" :underline t))))
 '(helm-grep-finish ((t (:foreground "#6CbCB9"))))
 '(helm-grep-lineno ((t (:foreground "#db322f"))))
 '(helm-grep-match ((t (:foreground "#db322f"))))
 '(helm-header ((t (:inherit default))))
 '(helm-header-line-left-margin ((t (:inherit default))))
 '(helm-history-remote ((t (:foreground "#D05E00" :slant italic))))
 '(helm-lisp-completion-info ((t (:inherit default))))
 '(helm-lisp-show-completion ((t (:inherit default))))
 '(helm-match ((t (:background "#ccccff" :foreground "#dc322f" :inverse-video t))))
 '(helm-prefarg ((t (:foreground "#c60007"))))
 '(helm-selection ((t (:inherit default :box (:line-width 1 :color "#81908f")))))
 '(helm-source-header ((t (:background "#52676f" :foreground "#fcf4dc" :weight bold))))
 '(highlight ((t (:inverse-video t))))
 '(isearch ((t (:background "#CCCCFF" :foreground "#dc322f" :inverse-video t :underline nil :slant normal :weight normal))))
 '(italic ((t (:slant italic))))
 '(latex-mode-default ((t (:inherit autoface-default :stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 150 :width normal :family "Menlo"))) t)
 '(lazy-highlight ((t (:background "#666666" :foreground "#fdf6e3" :weight normal))))
 '(line-number ((t (:inherit default :background "#042028" :foreground "#aaaaaa" :underline nil :weight thin))))
 '(line-number-current-line ((t (:foreground "gray50"))))
 '(match ((t (:background "RoyalBlue3" :foreground "gray80"))))
 '(mode-line ((t (:background "#bbbbbb" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground "gray30" :weight bold))))
 '(mode-line-inactive ((t (:inherit aquamacs-fixed-width :background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75") :strike-through nil :underline nil :slant normal :weight normal :width normal))))
 '(nobreak-space ((t (:foreground "cyan" :underline t))))
 '(org-agenda-done ((t (:foreground "#859901"))))
 '(org-agenda-structure ((t (:foreground "#278BD2"))))
 '(org-date ((t (:foreground "#278BD2" :underline t))))
 '(org-date-selected ((t (:foreground "#CB4B16" :inverse-video t))))
 '(org-document-info ((t (:foreground "#DC322F"))))
 '(org-document-info-keyword ((t (:inherit org-document-info))))
 '(org-document-title ((t (:foreground "#DC322f" :weight bold))))
 '(org-ellipsis ((t (:foreground "#CB4B16" :underline t))))
 '(org-footnote ((t (:foreground "#278BD2" :underline t))))
 '(org-formula ((t (:foreground "#CB4B16"))))
 '(org-habit-clear-face ((t (:background "#8270f9" :foreground "gray80"))))
 '(org-habit-overdue-face ((t (:background "#DC322F" :foreground "gray80"))))
 '(org-habit-overdue-future-face ((t (:background "IndianRed3" :foreground "wheat2"))))
 '(org-latex-and-related ((t (:foreground "#859901"))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :slant italic :weight bold))))
 '(org-scheduled ((t (:foreground "#2BA198"))))
 '(org-scheduled-today ((t (:foreground "#2BA198"))))
 '(org-sexp-date ((t (:foreground "#2187D2"))))
 '(org-table ((t (:foreground "#278BD2"))))
 '(org-time-grid ((t (:foreground "#2BA198"))))
 '(org-upcoming-deadline ((t (:foreground "#D33682"))))
 '(powerline-active1 ((t (:inherit mode-line))))
 '(powerline-active2 ((t (:inherit mode-line))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey60"))))
 '(secondary-selection ((t (:background "DarkGoldenrod3"))))
 '(success ((t (:foreground "#93a500" :weight bold))))
 '(tooltip ((t (:inherit variable-pitch :foreground "gray80"))))
 '(variable-pitch ((t (:family "Liberation Sans"))))
 '(warning ((t (:underline (:color "orange3" :style wave))))))

(provide '.emacs)
;;; .emacs ends here
