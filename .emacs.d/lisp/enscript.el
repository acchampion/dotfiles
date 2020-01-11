;;; enscript.el --- Joel's Custom printing functions
;; Author: Joel J. Adamson
;; Maintainer: Joel J. Adamson <sparrow1240 at gmail dot com>
;; version 0.1
;; keywords:  local wp tools
;; Copyright (C) 2007 Joel J. Adamson
;;
;; this file is NOT part of GNU Emacs
;; this file is NOT part of GNU Enscript
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA
;;; Code:

(defgroup enscript nil
  "Printing with GNU Enscript"
  :group 'printing
  :version "23.0.6.0")

;; TODO: create a condescending message for the compiler:
;; if the user isn't using unix: "You're not using Unix? Get on the bus, man!"
;; make an allowance for cygwin
;; if the user doesn't have Enscript: "Go get GNU Enscript, then call me back."

;; customize buffer printing font
;; TODO: If font can't be found, query
;; the user; this will require communication between the printing
;; process and the user

;; use watermark?
(defcustom use-underlay-p nil
  "Whether to use an underlay (watermark) when printing."
  :tag "Use Underlay?"
  :group 'enscript
  :type '(boolean))

;; watermark font
(defcustom underlay-font "Times-Roman100"
  "The standard font to print the underlay"
  :tag "Underlay Font"
  :group 'enscript
  :type '(string))

;; watermark text
(defcustom underlay-text user-full-name
  "The standard text to print as an underlay"
  :group 'enscript
  :type '(choice string sexp))

(defcustom line-numbers-p nil
  "Use line numbers when printing code summaries?"
  :tag "Print line numbers?"
  :group 'enscript
  :type '(boolean))

;; whether to use an Emacs-style header 
(defcustom use-fancy-header-p t
  "Whether to print an Emacs-style header"
  :tag "Use Fancy Header?"
  :group 'enscript
  :type '(boolean))

;; file to save in
;; TODO: launch previewer after successful printing
(defcustom preview-file nil
  "File for saving printed image"
  :group 'enscript
  :type '(choice (string :tag "File")
		 (const :tag "No preview" nil)))

(defcustom enscript-before-hook nil
  "Hook to run before printing"
  :type 'hook
  :group 'enscript)

(defcustom enscript-after-hook nil
  "Hook to run after printing"
;;; Example:     '(enscript-after-hook (quote ((lambda nil (when preview-file (shell-command (concat "gv " (expand-file-name preview-file) "&")))))))
;;; this opens gv for viewing as soon as the file prints
  :type 'hook
  :group 'enscript)


;; language highlighting

;; this list was built from the directory of /usr/share/enscript/hl
;; 
(defvar enscript-lang-collection
  '(("Ada" "ada")
    ("Asm" "asm")
    ("Awk" "awk")
    ("Bash" "bash")
    ("C" "c")
    ("Changelog" "changelog")
    ("C++" "cpp")
    ("C Shell" "csh")
    ("Default Faces" "default_faces")
    ("Delphi" "delphi")
    ("Diff" "diff")
    ("Emacs Lisp" "elisp")
    ("Fortran" "fortran")
    ("Fortran++" "fortran_pp")
    ("Haskell" "haskell")
    ("Html" "html")
    ("Idl" "idl")
    ("Inf" "inf")
    ("Java" "java")
    ("Javascript" "javascript")
    ("Korn Shell" "ksh")
    ("M4" "m4")
    ("Mail" "mail")
    ("Makefile" "makefile")
    ("Maple" "maple")
    ("Matlab" "matlab")
    ("Modula_2" "modula_2")
    ("Nested" "nested")
    ("Nroff" "nroff")
    ("Objective C" "objc")
    ("Outline Text" "outline")
    ("Pascal" "pascal")
    ("Passthrough" "passthrough")
    ("Perl" "perl")
    ("Postscript" "postscript")
    ("Python" "python")
    ("Rfc" "rfc")
    ("Scheme" "scheme")
    ("Bourne Shell" "sh")
    ("Skill" "skill")
    ("Sql" "sql")
    ("States" "states")
    ("Synopsys" "synopsys")
    ("Tcl" "tcl")
    ("Tenex C Shell" "tcsh")
    ("TeX" "tex")
    ("Visual Basic" "vba")
    ("Verilog" "verilog")
    ("Vhdl" "vhdl")
    ("Vrml" "vrml")
    ("Wmlscript" "wmlscript")
    ("Z Shell" "zsh")
    ("Don't use highlighting" nil))
  "Collection of languages for syntax highlighting using Enscript")
;; as per Emacs manual (info "(elisp) Basic Completion")
(setplist 'enscript-lang-collection '(risky-local-variable t))

;; collection of fonts
;; I need to build this list from the font.map files in the user and
;; system-wide configuration files (given in AFMPATH variable of ~/.enscriptrc)
;;
;; local user fonts are often kept in .fonts/
;; 
;; system-wide fonts are kept in /usr/share/enscript/afm
;;
;; you need afm files for these fonts: get tools to convert ttf to pfa, pfb and afm

(defvar enscript-font-collection
  '((Arimo-Bold                    	Arimo-Bold)
    (Arimo-BoldItalic              	Arimo-BoldItalic)
    (Arimo-Italic                  	Arimo-Italic)
    (Arimo                         	Arimo-Regular)
    (Cousine-Bold                  	Cousine-Bold)
    (Cousine-BoldItalic            	Cousine-BoldItalic)
    (Cousine-Italic                	Cousine-Italic)
    (Cousine                       	Cousine-Regular)
    (DejaVuSans-Bold               	DejaVuSans-Bold)
    (DejaVuSansCondensed-Bold      	DejaVuSansCondensed-Bold)
    (DejaVuSansCondensed-BoldOblique	DejaVuSansCondensed-BoldOblique)
    (DejaVuSansCondensed-Oblique   	DejaVuSansCondensed-Oblique)
    (DejaVuSansCondensed           	DejaVuSansCondensed)
    (DejaVuSansMono-Bold           	DejaVuSansMono-Bold)
    (DejaVuSansMono-BoldOblique    	DejaVuSansMono-BoldOblique)
    (DejaVuSansMono-Oblique        	DejaVuSansMono-Oblique)
    (DejaVuSansMono                	DejaVuSansMono)
    (Go-Bold-Italic                	Go-Bold-Italic)
    (Go-Bold                       	Go-Bold)
    (Go-Medium-Italic              	Go-Medium-Italic)
    (Go-Medium                     	Go-Medium)
    (Go-Regular-Italic             	Go-Regular-Italic)
    (Go-Regular                    	Go-Regular)
    (GoMono-BoldItalic             	GoMono-Bold-Italic)
    (GoMono-Bold                   	GoMono-Bold)
    (GoMono-Italic                 	GoMono-Regular-Italic)
    (GoMono                        	GoMono-Regular)
    (GoSmallcaps-Italic            	GoSmallcaps-Italic)
    (GoSmallcaps                   	GoSmallcaps)
    (BeraSansMono-Bold             	fvmb8a)
    (BeraSansMono-BoldOb           	fvmbo8a)
    (BeraSansMono-Roman            	fvmr8a)
    (BeraSansMono-Oblique          	fvmro8a)))

;; as per Emacs manual (info "(elisp) Basic Completion")
(setplist 'enscript-font-collection '(risky-local-variable t))

;; a typical location for the user's .enscriptrc
;;
;; I could use this to get the AFMPATH variable
;;
;; currently not used
(defvar enscript-user-enscript-file (expand-file-name "~/.enscriptrc"))

;; now the functions:
;; make a fancy title
(defun enscript-title-header (&optional fun)
  (cond
   ;; determine if this is a region-based printing command
   ;; 
   ;; TODO: this saves temporary files; it would be better to have a
   ;; more economical method of getting that friggin' title up there.
   ((string-match "region$" (symbol-name fun))
    (enscript-temp-file (current-buffer) fun))
   ((null (buffer-file-name))
    (enscript-temp-file (current-buffer) fun))
   ((buffer-modified-p (current-buffer))
    ;; query the user as to whether he'd like to save
    ;; if not, then save to a temporary file as aboave
    (if (yes-or-no-p "Save file? ")
	(progn
	  (save-buffer)
	  (buffer-file-name))
      (enscript-temp-file (current-buffer) fun)))
   (t (buffer-file-name))))

(defun enscript-temp-file (buffer &optional fun)
  (let* ((tmp (getenv "TMP"))
	 (filename (concat tmp (buffer-name buffer))))
    (cond ((string-match "region$" (symbol-name fun))
	   (write-region (point) (mark) filename nil 'inhibit))
	  (t (write-file filename)))
    filename))

;; format the options string:
(defun enscript-command-line (&optional font size lang fun)
  "Define the string of options for print commands"
  ;; produces a command line like this:
  ;;
  ;; enscript --ul-font=Times-Roman100 --ul-style=filled -G \
  ;; -fLuxiMono10 -C -E -opreview.ps  --style=a2ps
  (format "enscript -v %s %s %s %s %s %s --style=emacs --word-wrap --color=true --margins=36:36:36:44 -MLetter %s"
	  ;; underlay text
	  ;; underlay font
	  (if use-underlay-p
	      (concat " -u" underlay-text
		      " --ul-font=" underlay-font
		      " --ul-style=filled")
	    " ")
	  ;; fancy-header-p
	  (if use-fancy-header-p "-G" "")
	  ;; text font
	  (if (or (null font)
		  (null size)) "-fGoMono10"
	    (concat "-f" font size))
	  ;; use line numbers?
 	  (if line-numbers-p "-C" "")
	  ;; language
	  (if  lang
	      (concat "-E" lang)
	    " ")
	  ;; output file
	  (if preview-file (concat "-o " preview-file) " ")
	  (enscript-title-header fun)))

;; printing command
(defun enscript-command (start stop &optional font size lang fun)
  ;; abstracted printing command
  (run-hooks 'enscript-before-hook)
  ;;I should also communicate with the process and fix errors before
  ;; they progress to printing
  (shell-command-on-region start stop
			   (enscript-command-line font size lang fun))
  (run-hooks 'enscript-after-hook))

(defun enscript-input (langp)
  "Accepts user input and passes it back to printing commands"
  (cond (langp
	 (let ((lang-name (completing-read "Language: "
					   enscript-lang-collection nil t)))
	   (list lang-name)))
	(t (let ((font-name (completing-read "Font: "
					     enscript-font-collection))
		 (font-size (read-from-minibuffer "Size: ")))
	     (list font-name font-size)))))
  
;; font-print-buffer prints a buffer using a selected font
(defun font-print-buffer (font size)
  "Print a buffer using selected font.  See also
\\[font-print-region]"
  (interactive (enscript-input nil))
  (enscript-command (point-min) (point-max) font size nil 'font-print-buffer))

;; just like font-print-buffer but limited to a region
(defun font-print-region (font size)
  "Print a region using selected font.  See also
\\[font-print-buffer]"
  (interactive (enscript-input nil))
  (enscript-command (point) (mark) font size nil 'font-print-region))

;; print a buffer highlighted for a certain language with line numbers
(defun lang-print-buffer (lang)
  "Print a buffer highlighting a selected language.  See also
\\[font-print-buffer]"
  (interactive (enscript-input t))
  (let ((highlight (cadr (assoc lang enscript-lang-collection))))
    (enscript-command (point-min) (point-max) nil nil
		    highlight
		    'lang-print-buffer)))

(defun lang-print-region (lang)
  "Print a buffer highlighting a selected language.  See also
\\[font-print-buffer]"
  (interactive (enscript-input t))
  (let ((highlight (cadr (assoc lang enscript-lang-collection))))
    (enscript-command  (point) (mark) nil nil
		     highlight
		     'lang-print-region)))

(provide 'enscript)
 
