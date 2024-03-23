;;; .emacs --- Summary
;;;
;;; Author: Adam C. Champion
;;;
;;; Commentary:
;;;
;;; This is my personal Emacs configuration
;;;
;;; Code:
(setq select-enable-clipboard t)
(when (memq window-system '(mac ns x))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq ns-function-modifier 'meta))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)

(require 'package) ;; You might already have this line
(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("nongnu" . "https://elpa.nongnu.org/nongnu/" ) t))

(package-initialize) ;; You might already have this line

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)

;;; Install the esup package if you haven't already.
;; (unless (package-installed-p 'esup)
;;     (package-refresh-contents)
;;     (package-install 'esup))
(setq esup-depth 0)

(setq package-selected-packages
      '(flycheck which-key zenburn-theme json-mode auctex org
          smartparens exec-path-from-shell gnu-elpa-keyring-update
          eglot corfu vertico orderless savehist marginalia consult
          langtool treesit-auto ))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Fix path in shell.
;;
;; Code source: https://github.com/xenodium/dotsies/blob/main/emacs/
;;   features/fe-package-extensions.el
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
    (if (and (fboundp 'native-comp-available-p)
            (native-comp-available-p))
        (progn
            (message "Native comp is available")
            ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
            ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
            ;; Append to path to give priority to values from 
            ;;   exec-path-from-shell-initialize.
            (add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
            (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                       (when (getenv "LIBRARY_PATH")
                                           ":")
            ;; This is where Homebrew puts libgccjit libraries.
                                       (car (file-expand-wildcards
               (expand-file-name "/usr/local/opt/libgccjit/lib/gcc/*")))))
            ;; Only set after LIBRARY_PATH can find gcc libraries.
            (setq comp-deferred-compilation t)
            (setq comp-speed 3))
        (message "Native comp is *not* available")))


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Automatically apply theme (light or dark) based on OS theme
;; (also light or dark). This is for Emacs-Plus only:
;;   https://github.com/d12frosted/homebrew-emacs-plus .
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'leuven t))
    ('dark (load-theme 'leuven-dark t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)


(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

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
(add-hook 'TeX-mode-hook
  (lambda ()
    (setq TeX-command-extra-options "-shell-escape")
  )
)



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
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "english")
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


;; Turn on org mode when I'm editing org files.
;; Source: https://github.com/cocreature/dotfiles/
(use-package org
  :defer
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    ;; (global-set-key "\C-cl" 'org-store-link)
    ;; (global-set-key "\C-ca" 'org-agenda)
    (setq org-export-with-smart-quotes t)
    (setq user-mail-address "champion@cse.ohio-state.edu")
    (setq org-html-doctype "html5")
    (setq org-html-html5-fancy t)
    (require 'ox-publish)
    (require 'ox-slimhtml)
    ))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-font-lock-mode 1)


;; Org-mode method for generating abbreviations in HTML.
;; Source:
;;   https://emacs.stackexchange.com/questions/12624/how-can-i-generate-the-abbr-html-tag-from-org-mode
(defun org-export-abbr (backend)
  "Replace {abbreviation|description} to <abbr> html text in
current buffer"
  (when (equal backend 'html)
    (save-excursion
      (beginning-of-buffer)
      (replace-regexp "{\\(.*?\\)|\\(.*?\\)}"
                      "@@html:<abbr title=\"\\2\">\\1</abbr>@@"))))

(add-hook 'org-export-before-parsing-hook 'org-export-abbr)


;; Turn on ps-print.
(require 'lpr)
(require 'ps-print)

;; Enscript

(require 'enscript)

;; Add Cousine to supported printing fonts.
;; Make sure it's in your ~/.fonts directory.
;; Download it from:
;;   https://github.com/google/fonts/tree/main/apache/cousine
;;   (License: Apache 2.0)
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


(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :init
  (savehist-mode))

;; Enable the consult package.
;; Code from https://github.com/minad/consult
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (command (styles +orderless-with-initialism))
                                   (variable (styles +orderless-with-initialism))
                                   (symbol (styles +orderless-with-initialism)))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))



(use-package eglot
  :ensure t
  :commands eglot
  :hook
  ((c-mode . eglot-ensure)
   (cpp-mode . eglot-ensure)
   (markdown-mode . eglot-ensure)
   (org-mode . eglot-ensure)
   (python-mode . eglot-ensure))
   ;;(text-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs
               '((markdown-mode) "efm-langserver"))
  (add-to-list 'eglot-server-programs
               '((org-mode) "efm-langserver")))


(use-package which-key
    :config
    (which-key-mode))
;; (use-package hydra)


;; Corfu (Emacs completion)
;; Source: https://github.com/minad/corfu
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (setq tab-always-indent 'complete)
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(require 'popup)

;; Set portion of heap used for garbage collection to 60%.
;; Source: Joe Schafer,
;;   https://github.com/jschaf/dotfiles/blob/master/emacs/start.el
(setq gc-cons-percentage 0.6)

(setq gc-cons-threshold (* 256 1024 1024)
      read-process-output-max (* 4 1024 1024)
      treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.250
;;       company-tooltip-limit 10
;;       company-echo-delay 0
;;       company-show-numbers nil
      lsp-prefer-capf t
;;      company-minimum-prefix-length 1
      create-lockfiles nil
      lsp-idle-delay 0.100 ;; clangd is fast
      ;; be more ide-ish
      lsp-headerline-breadcrumb-enable t
      lsp-use-plists t
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

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (latex-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode)
         (text-mode . flycheck-mode))
  :config
  (setq flycheck-chktexrc "~/.chktexrc")
;;  (require 'flycheck-vale)
;;  (flycheck-vale-setup)
  (setq flycheck-vale-program "/usr/local/bin/vale")
)

(blink-cursor-mode 0)
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; (global-linum-mode t) ;; enable line numbers globally
;; (setq linum-format "%d ")

;; (require 'perl-use-utf8-coding)

(setq speedbar-show-unknown-files t)

;; Set fonts for macOS and Linux. I use Office Code Pro or
;; Liberation Mono.
;; Source: https://www.emacswiki.org/emacs/SetFonts (cinsk)
;; default font size (point * 10)
;;
;; WARNING!  Depending on the default font,
;; if the size is not supported very well, the frame will be clipped
;; so that the beginning of the buffer may not be visible correctly.

(when (eq system-type 'windows)
  (set-face-attribute 'default nil :font "Consolas")
  (set-face-attribute 'default nil :height 160)
)

(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :font "Office Code Pro D")
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
;; Enable global line highlighting.
(global-hl-line-mode 1)
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
(setq use-short-answers 1)
(tool-bar-mode -1)
;; (setq garbage-collection-messages t)


;; Joe Schafer's profiling code:
;; https://blog.d46.us/advanced-emacs-startup/
;;
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
      (format "%.2f seconds"
        (float-time
          (time-subtract after-init-time before-init-time)))
            gcs-done)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "ACrMBP16.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("e3c41651565cb624f772d25fbf12752b31610800041968d96c9aef5a3e8ead8e" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "9e3ea605c15dc6eb88c5ff33a82aed6a4d4e2b1126b251197ba55d6b86c610a1" "569bc616c09c389761622ca5be12031dcd7a0fe4c28b1b7154242812b694318c" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(package-selected-packages
   '(org treesit-auto adwaita-dark-theme flycheck-yamllint yaml-mode flycheck-vale tree-sitter-langs tree-sitter corfu-terminal eglot-jl eglot-java eglot emmet-mode web-mode corfu esup sr-speedbar latex-preview-pane auctex-latexmk pdf-tools lsp-java lsp-origami find-file-in-project company-ctags lsp-pyright company-lsp lsp-ui which-key zenburn-theme use-package solarized-theme smartparens projectile langtool gnu-elpa-keyring-update flycheck exec-path-from-shell elpy auctex))
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
