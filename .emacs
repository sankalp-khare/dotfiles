;; Sankalp's .emacs

;; note: I find add-to-list more robust and elegant
;;       (compared to cons+setq)

;; TODOs
;; Reduce clutter in $HOME .
;; Still to figure a way to shift these to .emacs.d
;; - .eshell
;; - .tramp_history

;; ---------------------------------------------------------
;; =========================================================
;; Level 1 : Stuff that should just work out of the box...
;; =========================================================
;; ---------------------------------------------------------

;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;; disabled because of too many queries getting irksome
;; (setq require-final-newline 'query)

;; Display time with day and date
(setq display-time-day-and-date t)
(display-time)

;; turn on visual bell
(setq visible-bell t)

;; display line and column number in "status bar" (mode line)
(line-number-mode 1)
(column-number-mode 1)

;; display file size in mode line
(setq size-indication-mode t)

;; set nu
;; (global-linum-mode)

;; set initial scratch buffer message
(setq initial-scratch-message "# got an itch?\n# scratch it!\n\n")

;; Highlight selected regions
(transient-mark-mode t)

;; If at beginning of a line, don't make me C-k twice.
(setq kill-whole-line t)

;; move between buffers using M-arrowkey
;; (windmove-default-keybindings 'meta)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; No Menu Bar
(menu-bar-mode -1)

;; No Toolbar
;; (tool-bar-mode -1)

;; even more hardcore, no scrollbars
(scroll-bar-mode -1)

;; don't ask when I press C-x k
;; http://superuser.com/a/355105
;; (global-set-key "\C-xk" 'kill-this-buffer)

;; remember location in files
(require 'saveplace)
(setq save-place-file "~/.emacs.d/emacs-places")
(setq-default save-place t)

;; auto-revert (read from disk) files that changed on disk, but haven't been
;; edited by user
(global-auto-revert-mode t)

;; start the emacs server if running with X and if no other
;; instance has a server running
;; useful if emacs is to be specified as default SVN editor
(when window-system
  (unless (boundp 'server-process) ; why does this check not work??
    (server-start)))

;; don't show the startup message every time
;; I know I'm using emacs, thanks
(setq inhibit-startup-message t)

;; http://news.ycombinator.com/item?id=1654619
;; Display this instead of "For information about GNU Emacs and the
;; GNU system, type C-h C-a.". This has been made intentionally hard
;; to customize in GNU Emacs so I have to resort to hackery.
(defun display-startup-echo-area-message ()
  "If it wasn't for this you'd be GNU/Spammed by now"
  (message "Hi, Welcome to Sankalp's Emacs!"))

;; http://news.ycombinator.com/item?id=1654619
;; Don't insert instructions in the *scratch* buffer
;; (setq initial-scratch-message nil)

;; don't blink the cursor
(blink-cursor-mode -1)

;; Highlight matching parentheses
(show-paren-mode 1)

;; remove delay in showing parentheses
(setq show-paren-delay 0)

;; dunno what this does yet
(setq show-paren-style 'mixed)

;; parenthesis jumping
;; http://emacs-fu.blogspot.in/2009/01/balancing-your-parentheses.html?showComment=1232229900000#c8552882987467504180
(defun paren-jump ()
  "Tries to jump to the matching parenthesis to the one currently
under the point. Useful if the matching paren is out of sight. "
  (interactive)
  (cond ((looking-at "[{\[\(]") (forward-sexp 1) (backward-char))
        ((looking-at "[]})]") (forward-char) (backward-sexp 1))
        (t (message "Point not at a parenthesis."))))

(global-set-key (kbd "C-x C-p") 'paren-jump)


;; ediff buffers sid-by-side, like it should
(setq ediff-split-window-function 'split-window-horizontally)

;; auto-complete on the ielm repl
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)


;; ---------------------------------------------------------
;; Dired customizations
;; ---------------------------------------------------------

;; dired directory delete/copy behaviour customization
(setq
 dired-recursive-deletes 'always
 dired-recursive-copies  'always
 )

;; automatically prompt to copy/move file to other dired-ed window
(setq dired-dwim-target t)

;; ---------------------------------------------------------
;; Font customizations
;; ---------------------------------------------------------

;; font size
(set-face-attribute 'default nil :height 160)

;; Set font (i'm using it to set the size only)
;; get the details of the current config by going to *scratch*,
;; setting things up the way you want, and then running
;; (pp (current-frame-configuration))^J
;;-----------------------------------
;; :deprecated: (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1")
;; (add-to-list 'default-frame-alist '(font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1"))
;; (set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1")

;; ---------------------------------------------------------

;; turn on flyspell-mode in LaTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; make flyspell operate as per British English
(ispell-change-dictionary "british" t)

;; ido mode
(ido-mode t)
(setq
 ido-enable-flex-matching t      ; be flexible
 ido-everywhere t
 ido-confirm-unique-completion t ; wait for RET, even with unique completion
 ido-save-directory-list-file "~/.emacs.d/ido.last"
 ido-case-fold  t                ; be case-insensitive
 )

;; visual line mode
;; word wrap works fine with auto-fill mode but removing this
;; breaks my hungry backspace setup :(
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 78)

;; auto-enable flyspell for text modes
;; (add-hook 'text-mode-hook 'turn-on-flyspell)

;; disable gui file dialog
(setq use-dialog-box nil)

;; allow typing y/n instead of yes/no in most cases
(fset 'yes-or-no-p 'y-or-n-p)

;;set the keybinding so that f3 will start the shell
(global-set-key [f3] 'shell)

;; http proxy
;; (setenv "http_proxy" "http://proxy.iiit.ac.in:8080")

;; auto-indent on newline
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'sh-mode-hook 'set-newline-and-indent)
;; (add-hook 'cc-mode-hook 'set-newline-and-indent)
(add-hook 'c-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'set-newline-and-indent)
(add-hook 'scheme-mode-hook 'set-newline-and-indent)
(add-hook 'shell-script-mode-hook 'set-newline-and-indent)
(add-hook 'emacs-lisp-mode-hook 'set-newline-and-indent)
(add-hook 'LaTeX-mode-hook 'set-newline-and-indent)
(add-hook 'org-mode-hook 'set-newline-and-indent)

;; c/c++ better indentation style
;; http://stackoverflow.com/a/664492
(setq c-default-style "bsd" c-basic-offset 4)
;; (defun my-c-mode-common-hook ()
;;   ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
;;   (c-set-offset 'substatement-open 0)
;;   ;; other customizations can go here

;;   (setq c++-tab-always-indent t)
;;   (setq c-basic-offset 4)                  ;; Default is 2
;;   (setq c-indent-level 4)                  ;; Default is 2

;;   (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
;;   (setq tab-width 4)
;;   (setq indent-tabs-mode t))  ; use spaces only if nil
;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; backspace removes tabs in a single go, like in vim -- works in cc-mode only
(setq backward-delete-char-untabify-method (quote hungry))

;; ---------------------------------------------------------
;; Colours and Colour Themes
;; ---------------------------------------------------------

;; Colour customizations
(defvar mytheme-setting "dark"
  "the default theme")

(defun my-toggle-theme()
  (interactive)
  (if (selected-frame)
      (progn
        ;; (print mytheme-setting)
        (if (string= mytheme-setting "dark")
            (setq mytheme-setting "light")
          (if (string= mytheme-setting "light")
              (setq mytheme-setting "dark")))
        ;; (print mytheme-setting)
        (my-frame-color-customizations (selected-frame)))))

;; customizations are defined here
(defun my-frame-color-customizations(frame)
  (if window-system
      (progn
        (select-frame frame)
        (if (string= mytheme-setting "dark")
            (progn
              (set-background-color "black")
              (set-foreground-color "grey")
              (set-cursor-color "grey")
              (set-mouse-color "grey")))
        (if (string= mytheme-setting "light")
            (progn
              (set-background-color "white")
              (set-foreground-color "black")
              (set-cursor-color "black")
              (set-mouse-color "black"))))))

;; function that applies the currently set customizations on
;; the current frame
(defun my-apply-color-customizations()
  (cond ((selected-frame)
         (my-frame-color-customizations (selected-frame)))))

;; call this on startup
;; (my-apply-color-customizations)

;; ensure new frames have these colours too
;; (add-hook 'after-make-frame-functions 'my-frame-color-customizations)

;; add a way to undo color themes
(defun color-theme-undo ()
  (interactive)
  (color-theme-reset-faces)
  (color-theme-snapshot))
;; backup current color theme
(fset 'color-theme-snapshot (color-theme-make-snapshot))
;; color themes can now be undone using M-x color-theme-undo

;; ---------------------------------------------------------

;; use the parent dir name to distinguish b/w buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; EasyPG
;; seamless .gpg file handling
(require 'epa-file)
;; (epa-file-enable) ; not needed. Happens on loading epa-file
(setenv "GPG_AGENT_INFO" nil) ; prevent GUI Password Prompt

;; Autoinsert
(require 'autoinsert)
(auto-insert-mode)  ; Adds hook to find-files-hook
(setq auto-insert-query nil) ; don't prompt before insertion
;; autoinsert templates
(setq auto-insert-directory "~/.emacs.d/templates/") ; Trailing slash important
(define-auto-insert "\\.py" "python-template.py")
(define-auto-insert "\\.sh" "bash-template.sh")
(define-auto-insert "\\.org" "org-template.org")

;; Make scripts executable automagically when saved
;; works intelligently (e.g. skips C/C++ source code)
(require 'executable)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Kills all them buffers except scratch
;; copied from   http://www-cdf.fnal.gov/~sthrlnd/emacs_help.html
;; obtained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
          (buffer-list))
  (delete-other-windows))

;; For maximize (NOT full screen!) on starting Emacs
;; taken from Prof.Choppella's dotemacs, function renamed appropriately
(defun toggle-maximize ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(when window-system
  (toggle-maximize))

;; toggle full screen, bind to [f11]
;; http://www.emacswiki.org/emacs/FullScreen#toc17
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

;; function to indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; functions to insert time, date etc Orgmode style :)
(defun insert-time ()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %R]")))
(defun insert-date ()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a]")))

;; ---------------------------------------------------------
;; Autosaves and Backups
;; ---------------------------------------------------------

;; Put autosave files (ie #foo#) in one place
;; *not* scattered all over the file system!
(defvar autosave-dir "~/.emacs.d/autosaves/")
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms
      `((".*" ,autosave-dir t)))

;; Put backup files (ie foo~) in one place too.
;; Emacs will mkdir it if necessary.
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;; ---------------------------------------------------------
;; Stuff written by me... Not all useful
;; ---------------------------------------------------------

;; function to save and close the buffer (:wq)
;; figured it out with Mihir
(defun save-close ()
  (save-buffer)
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x K") 'save-close)

;; My attempt :(
;; (defun swap-buffers ()
;;   "swap the two current buffers (frames actually)"
;;   (interactive)
;;   (let ((this (current-buffer)))
;;     (progn
;;       (other-window 1)
;;       (delete-other-windows)
;;       (switch-to-buffer-other-window this))))

;; working swap function
;; http://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

;; ---------------------------------------------------------
;; =========================================================
;; Level 2 : Stuff that needs stuff which can be found in
;;           the yum/apt repositories
;; =========================================================
;; ---------------------------------------------------------

;; opening pdf at current cursor position
(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '(((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open")))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)

;; Ebib BibTeX database manager
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(setq ebib-preload-bib-files
      '("~/Documents/SERL/rsys-svn/Reference Material/refs.bib"))

;; auto-complete-el
;; these lines are not required on Fedora systems
(require 'auto-complete-config)
(ac-config-default)

;; psvn.el for better svn integration
;; (ships with the subversion package in Fedora/Ubuntu)
;; (subverison 1.7 and onwards do not have it (yet)!)
;; M-x svn-status to add a whole new dimension to your svn-ing!
(require 'psvn)
;; run 'svn status' instead of 'svn status -v' in svn-status
(setq svn-status-verbose nil)

;; ---------------------------------------------------------

;; ---------------------------------------------------------
;; =========================================================
;; Level 3 : Stuff that needs stuff which isn't found in the
;;           yum/apt repositories and has to be downloaded
;;           into ~/.emacs.d/
;; =========================================================
;; ---------------------------------------------------------

;; ---------------------------------------------------------
;; package.el
;; ---------------------------------------------------------

;; Check if the installer gives you the latest version of package.el . If not,
;; use this link to get the latest version and replace

;; https://github.com/technomancy/package.el
;; http://bit.ly/pkg-el23 (latest version compatible with emacs23.x)

;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.elc"))
;;   (package-initialize))

(add-to-list 'load-path "~/.emacs.d/elpa/")
(require 'package)

;; ---------------------------------------------------------
;; [ELPA] Marmalade (Another source for package.el)
;; http://marmalade-repo.org/
;; ---------------------------------------------------------

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))

;; ---------------------------------------------------------
;; [ELPA] MELPA (Another source for package.el)
;; http://melpa.milkbox.net
;; ---------------------------------------------------------

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))

;; Adding the org-mode repo to package.el
;; http://orgmode.org/elpa.html
;; (require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; initialize package.el
(package-initialize)

;; ---------------------------------------------------------
;; [ELPA] python-mode
;; ---------------------------------------------------------

;; Somehow the ELPA install is not auto-detected, so in the absence of the
;; following two lines, require 'python-mode still loads the python.el that
;; comes with emacs
(add-to-list 'load-path (concat (car (file-expand-wildcards "~/.emacs.d/elpa/python-mode-*")) "/"))
(setq py-install-directory (concat (car (file-expand-wildcards "~/.emacs.d/elpa/python-mode-*")) "/"))
;; (setq py-load-pymacs-p t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
;; disable python.el
(when (featurep 'python) (unload-feature 'python t))
;; enable python-mode
(require 'python-mode)
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ;; hacks for desired behaviour of py-execute-region
;; ;; this prevents py-execute-region eclipsing the python file buffer
;; ;; Use around-advice to wrap the function in a call to save-window-excursion,
;; ;; which will restore the previous window configuration after the command completes.
;; (defadvice py-execute-region
;;   (around preserve-window-configuration activate)
;;   "After execution, return cursor to script buffer"
;;   (save-window-excursion ad-do-it))
;; ;; Keep in mind, though, that if the Python buffer wasn't already shown,
;; ;; it will still be hidden after the command completes.
;; ;; To remedy that, you can add another piece of advice to call
;; ;; switch-to-buffer-other-window at the end
;; (defadvice py-execute-region
;;   (after show-pybuf-other-window activate)
;;   "After execution, show the python buffer in another window."
;;   (switch-to-buffer-other-window "*Python*"))
;; ;; after the execution of the two, the cursor actually ends up in the shell

;; [[racket-mode obsoletes this block]]
;; [ELPA] quack mode for scheme programming
;; (add-to-list 'load-path (concat (car (file-expand-wildcards "~/.emacs.d/elpa/quack-*")) "/"))
;; (require 'quack)
;; enter scheme-mode while editing .rkt files
;; (setq auto-mode-alist
;;       (cons '("\\.rkt$" . scheme-mode)
;;             auto-mode-alist))
;; set mzscheme as the default scheme interpreter
;; (setq scheme-program-name "racket")
;; do not query for interpreter name if nothing is passed -- load the default
;; (setq quack-run-scheme-always-prompts-p nil)
;; pretty lambda ain't working :(
;; (setq quack-pretty-lambda-p t)

;; ---------------------------------------------------------
;; [ELPA] Org-mode
;; ---------------------------------------------------------

;; set default major mode for the scratch buffer
;; this line has to occur *after* the package.el initialization, since org is
;; now being sourced from there
(setq initial-major-mode 'org-mode)

;; use org-7.8.03
;; from Emacs 24 this will become unnecessary as orgmode is
;; being inducted into Emacs Core. #win
;; (add-to-list 'load-path "~/.emacs.d/org-7.8.03/lisp/")
;; (add-to-list 'load-path "~/.emacs.d/org-7.8.03/contrib/lisp/")

;; recommended orgmode customizations (from the Manual)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; my orgmode customizations
(setq
 org-hide-leading-stars t              ;; only show a single (albeit indented) star regardless of heading level
 org-return-follows-link t             ;; open hyperlinks by pressing RET on them
 org-completion-use-ido t              ;; use ido completion wherever it makes sense
 org-blank-before-new-entry            ;; never insert blank lines
 '((heading . nil)
   (plain-list-item . nil))
 org-insert-heading-respect-content t  ;; make new headings appear after the content for the current one
 org-log-done 'time                    ;; logs a timestamp when an entry is marked DONE
 org-log-done 'note                    ;; prompts for a note at closing
 org-publish-timestamp-directory
 (convert-standard-filename            ;; reduce clutter in homedir
  "~/.emacs.d/org-timestamps/")
 org-directory "~/.emacs.d/org-files/" ;; default org directory for files
 org-default-notes-file                ;; default file for captured notes
 (concat org-directory "notes.org")
 )

;; some templates for org-capture
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/.emacs.d/org-files/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry
         (file+datetree "~./emacs.d/org-files/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Default template" entry
         (file+headline "~./emacs.d/org-files/capture.org" "Notes")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)))

;; turn on flyspell-mode in Org
(add-hook 'org-mode-hook 'flyspell-mode)

;; make org use evince for pdfs
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . evince)))

;; for latex exports
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
;; to allow LaTeX_CLASS: IEEEtran
;; (require 'org-latex)
;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))
(unless (find "IEEEtran" org-export-latex-classes :key 'car
              :test 'equal)
  (add-to-list 'org-export-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran}
                 [NO-DEFAULT-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; per-file-class with minimal packages
(unless (find "per-file-class" org-export-latex-classes :key 'car
              :test 'equal)
  (add-to-list 'org-export-latex-classes
               '("per-file-class"
                 "\\documentclass{article}
                [NO-DEFAULT-PACKAGES]
                [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Babel
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (latex . t)
   (sh . t)
   (emacs-lisp . t)
   (C . t)
   (latex . t)
   (ditaa . t)
   (dot . t)
   ))

;; Basic setup is complete
;; don't ask each time I compile/run something
(setq org-confirm-babel-evaluate nil)
;; syntax highlighting right within the orgmode buffer!
(setq org-src-fontify-natively t)

;; load org-protocol.el
(require 'org-protocol)

;; org-crypt to encrypt sections in files
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;; [ELPA] org2blog :: https://github.com/punchagan/org2blog
(require 'org2blog-autoloads)
;; NOT WORKING :: TO BE FIXED! [2013-01-09 Wed 09:30]
(setq org2blog/wp-blog-alist
      '(("crackysblog"
         :url "http://sankalpkhare.wordpress.com/xmlrpc.php"
         :username "crackysblog"
         :default-title ""
         :default-categories ()
         :tags-as-categories nil)
        ))

;; [ELPA] auctex
(require 'tex-site)
(setq-default TeX-master t)
(setq TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)

;; change RET behaviour in AUCTeX
(setq-default TeX-newline-function 'newline-and-indent)

;; compile documents to PDF by default
(setq TeX-PDF-mode t)

;; enable RefTeX (in AUCTeX)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; integrate RefTeX with AUCTeX
(setq reftex-plug-into-AUCTeX t)

;; [ELPA] smooth-scrolling | http://www.adamspiers.org/elisp/smooth-scrolling.el
(require 'smooth-scrolling)
(setq smooth-scroll-margin 6)

;; hungry-delete mode
;; backspace removes tabs in a single swipe
;; note: the version I use is a slightly modified version
;;       read comments in the source for details
(add-to-list 'load-path "~/.emacs.d/hungry-delete/")
(require 'hungry-delete)
(global-hungry-delete-mode)

;; [ELPA] autopair for automatic parenthesis closing
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; [ELPA] openwith minor mode
;; for opening pdfs in evince, mp3s in some player etc...
(require 'openwith)
(setq openwith-associations
      '(
        ("\\.pdf\\'"  "evince"  (file))
        ("\\.ps\\'"   "evince"  (file))
        ("\\.mp3\\'"  "clementine"  (file))
        ("\\.doc\\'"  "libreoffice" (file))
        ("\\.docx\\'" "libreoffice" (file))
        ("\\.xls\\'"  "libreoffice" (file))
        ("\\.xlsx\\'" "libreoffice" (file))
        ("\\.ppt\\'"  "libreoffice" (file))
        ("\\.pptx\\'" "libreoffice" (file))
        ("\\.nfo\\'"  "nfoview" (file))
        ("\\.torrent\\'"  "qbittorrent" (file))
        ("\\.flv\\'"  "vlc" (file))
        ("\\.avi\\'"  "vlc" (file))
        ("\\.mp4\\'"  "vlc" (file))
        ("\\.mkv\\'"  "vlc" (file))
        ("\\.mpg\\'"  "vlc" (file))
        ("\\.mpeg\\'"  "vlc" (file))
        ("\\.vob\\'"  "vlc" (file))
        ("\\.divx\\'"  "vlc" (file))
        ("\\.jpg\\'"  "eog" (file))
        ("\\.jpeg\\'"  "eog" (file))
        ("\\.png\\'"  "eog" (file))
        ("\\.gif\\'"  "eog" (file))
        ("\\.svg\\'"  "inkscape" (file))
        ;; ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "vlc" ("-idx" file))
        ;; ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))
        ))
(openwith-mode t)

;; [ELPA] notify.el
;; send desktop notifications from Emacs
;; (require 'notify)
;; (autoload 'notify "notify" "Notify TITLE, BODY.")

;; smart-compile+ for easy compilation and execution of c/c++ and other codes
;; http://www.emacswiki.org/emacs/smart-compile+.el
(add-to-list 'load-path "~/.emacs.d/smart-compile/")
(require 'smart-compile+)
(global-set-key [f5] 'smart-compile)

;; [ELPA] Solarized Color Theme for Emacs
;; http://ethanschoonover.com/solarized
;; https://github.com/sellout/emacs-color-theme-solarized
;; (when window-system
;;   (progn
;;     (require 'color-theme-solarized)
;;     (color-theme-solarized-dark)))

;; ---------------------------------------------------------
;; =========================================================
;; Level 4 : Stuff that needs to be at the end of the .emacs
;;           for various reasons
;; =========================================================
;; ---------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/home/sankalp/tmp/qbittorrent/qBittorrent")))
 '(quack-programs (quote ("racket" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; compile .emacs to boost startup speed
;; will help as my .emacs gets larger...
;; (defun autocompile-dotemacs nil
;;   "compile itself if ~/.emacs"
;;   (interactive)
;;   (require 'bytecomp)
;;   (if (string= (buffer-file-name) (expand-file-name (concat
;;                                                      default-directory ".emacs")))
;;       (byte-compile-file (buffer-file-name))))
;; (add-hook 'after-save-hook 'autocompile-dotemacs)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
