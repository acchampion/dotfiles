;;; Adam's .emacs file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq select-enable-clipboard t)

(when (memq window-system '(mac ns x))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq ns-function-modifier 'meta))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(scroll-bar-mode -1)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("org" . "https://orgmode.org/elpa/") t)

(package-initialize) ;; You might already have this line

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq package-selected-packages
  '(lsp-mode yasnippet lsp-treemacs helm-lsp lsp-ui projectile hydra
    flycheck company avy which-key helm-xref dap-mode lsp-java
    zenburn-theme json-mode lsp-pyright auctex org langtool
    smartparens exec-path-from-shell gnu-elpa-keyring-update
    latex-preview-pane ))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(load-theme 'zenburn)

;; Set up "useful" coding environment.
;; Source: https://github.com/tuhdo/emacs-c-ide-demo
(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)


;; Automatically turn on references, math mode with AUCTeX.
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;; (latex-preview-pane-enable)

(autoload 'longlines-mode
   "longlines.el"
   "Minor mode for automatically wrapping long lines." t)

(setenv "PATH"
      (concat "/usr/local/lib" ":"
        (getenv "PATH")))

(setenv "TEXMFMAIN"
	(concat "/usr/local/texlive/texmf-local" ":"
		(concat "/usr/local/texlive/2022/texmf-dist"
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
(setq org-export-with-smart-quotes t)
(setq user-mail-address "champion@cse.ohio-state.edu")
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

;; Turn on ps-print.
(require 'lpr)
(require 'ps-print)
(require 'enscript)

;; Add Cousine to supported printing fonts.
;; Make sure it's in your ~/.fonts directory.
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


;; Turn off shell mode bold text from previous commands.
(setq comint-highlight-input nil)


;; Langtool: fix spelling errors for text mode
(require 'langtool)
(when (memq window-system '(mac ns x))
  (setq langtool-java-tool-jar "/usr/local/opt/languagetool/libexec/languagetool.jar")
  (setq langtool-bin "/usr/local/bin/languagetool"))
(setq langtool-default-language "en-US")
(setq langtool-mother-tongue "en")
(setq sentence-end-double-space nil)


;; Programming
(setq c-default-style "linux") ; set style to "linux"
(setq c-basic-offset 2)
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; C programming support for emacs
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
;; (require 'semantic)
(setq compilation-ask-about-save nil          ; Just save before compiling
      compilation-always-kill t               ; Just kill old compile processes before starting new ones
      compilation-scroll-output 'first-error) ; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; Source: https://github.com/tuhdo/emacs-c-ide-demo
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (global-semantic-stickyfunc-mode 1)

;; (semantic-mode 1)

;; (defun alexott/cedet-hook ()
;;   (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
;;   (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))
;;
;; (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
;; (add-hook 'c-mode-hook 'alexott/cedet-hook)
;; (add-hook 'c++-mode-hook 'alexott/cedet-hook)
;; (add-hook 'c++-mode-hook 'irony-mode)
;;
;; ;; Enable EDE only in C/C++
;; (require 'ede)
;; (global-ede-mode)

;; GDB
 ;; use gdb-many-windows by default
 ;; Display source file containing the main routine at startup
(setq gdb-many-windows t)
(setq gdb-show-main t)


;; Helm settings from https://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-xref)
(helm-mode t)
(use-package helm-lsp)
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
       helm-ff-skip-boring-files nil)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match nil) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "ack")
   (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
         helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(add-hook 'eshell-mode-hook
           #'(lambda ()
               (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)


;; LSP mode: provide consistent language servers to check for errors
;;   in C, C++, HTML, CSS, ECMAScript, etc.
(use-package lsp-mode
  :commands lsp
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (css-mode . lsp-deferred)
  (html-mode . lsp-deferred)
  (java-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (sh-mode . lsp-deferred) )

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package which-key
    :config
    (which-key-mode))

(use-package hydra)

;; Speed up LSP mode.
;; Source: http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (require 'dap-chrome)
  (dap-auto-configure-mode)
  (yas-global-mode)

 ;; enable log only for debug
  (setq lsp-log-io nil)

  (setq lsp-enable-completion-at-point t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-links nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-client-packages '(lsp-clients))

  ;; don't ping LSP lanaguage server too frequently
  (defvar lsp-on-touch-time 10)
  (defadvice lsp-on-change (around lsp-on-change-hack activate)
    ;; don't run `lsp-on-change' too frequently
    (when (> (- (float-time (current-time))
                lsp-on-touch-time) 10) ;; 10 seconds
      (setq lsp-on-touch-time (float-time (current-time)))
      ad-do-it))
  )


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

(setq gc-cons-threshold (* 256 1024 1024)
      read-process-output-max (* 4 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.001
      company-tooltip-limit 10
      company-echo-delay 0
      company-show-numbers nil
      lsp-prefer-capf t
      company-minimum-prefix-length 1
      create-lockfiles nil
      lsp-idle-delay 1.000 ;; clangd is fast
      ;; be more ide-ish
      lsp-headerline-breadcrumb-enable t
      lsp-log-io nil)


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

;; (elpy-enable)
;; (setq elpy-rpc-python-command "python3")

;; (require 'perl-use-utf8-coding)

(setq speedbar-show-unknown-files t)
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; Set fonts for macOS and Linux. I use Menlo, Office Code Pro, or
;; Liberation Mono.
;; Source: https://www.emacswiki.org/emacs/SetFonts (cinsk)
(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :font "Office Code Pro")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 160)
)
(when (eq system-type 'linux)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :font "Liberation Mono")
  (set-face-attribute 'default nil :height 160)
)

;; Set "custom" variables so we don't depend on Custom in Emacs.
(setq blink-cursor-mode nil)
(setq column-number-mode t)
(when (eq system-type 'darwin)
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
)
(setq inhibit-startup-screen t)
(setq line-numbers-p t)
(setq org-agenda-export-html-style nil)
(setq org-export-html-style-include-default nil)
(setq org-highlight-latex-and-related (quote (latex script entities)))
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq preview-file "emacs-enscript.ps")
(show-paren-mode t)
(size-indication-mode t)
(setq speedbar-default-position (quote left))
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-right-side nil)
(tool-bar-mode -1)
;; (setq garbage-collection-messages t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "9e3ea605c15dc6eb88c5ff33a82aed6a4d4e2b1126b251197ba55d6b86c610a1" "569bc616c09c389761622ca5be12031dcd7a0fe4c28b1b7154242812b694318c" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(package-selected-packages
   '(sr-speedbar latex-preview-pane auctex-latexmk pdf-tools lsp-java lsp-origami find-file-in-project company-ctags lsp-pyright dap-mode company-lsp lsp-ui which-key helm-lsp helm-xref lsp-treemacs lsp-mode zenburn-theme use-package solarized-theme smartparens projectile langtool helm-gtags gnu-elpa-keyring-update flycheck exec-path-from-shell elpy auctex))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide '.emacs)
;;; .emacs ends here
