;; Sankalp's .emacs
;; Version 2; emacs >=24

;; note: I find add-to-list more robust and elegant
;;       (compared to cons+setq)

;; TODOs
;; Reduce clutter in $HOME .
;; Still to figure a way to shift these to .emacs.d
;; - .eshell
;; - .tramp_history

;;=============================
;; Emacs 24 Updates:
;;
;;  - most stuff installed via package.el (which is now integrated
;;    into Emacs core) gets auto-loaded from ~/.emacs.d/elpa/
;;
;;  -
;;
;;
;;
;;

;; Mac Specific Customizations | jhamrick
;; http://www.jesshamrick.com/2013/03/31/macs-and-emacs/
(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

;; set command key to be meta instead of option
(if (system-is-mac)
    (setq ns-alternate-modifier 'super
          ns-command-modifier 'meta))

;; themes directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; load material theme
;; (load-theme 'material t)

;; load monokai theme
(load-theme 'monokai t)

;; package.el
(package-initialize)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; default to unified diffs
(setq diff-switches "-u")

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

;; set vertical split as default
;; http://stackoverflow.com/a/7998271
(setq split-width-threshold nil)

;; set initial scratch buffer message
(setq initial-scratch-message "# got an itch?\n# scratch it!\n\n")

;; set default major mode for the scratch buffer
(setq initial-major-mode 'org-mode)

;; Highlight selected regions
(transient-mark-mode t)

;; If at beginning of a line, don't make me C-k twice.
(setq kill-whole-line t)

;; insert a newline at the end of a file if not present, at the time of saving
(setq-default require-final-newline t)

;; clear trailing whitespaces in lines at the time of saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; reuse external ssh controlpath (reuse connection)
;; vital after 2fa enabling on mobolt workstations
;; (setq tramp-ssh-controlmaster-options
;;       (concat
;;        "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
;;        "-o ControlMaster=auto -o ControlPersist=yes"))
;; DOESNT WORK

;; edit-server.el for chrome text box editing requests from plugins
;; plugin i am using at this time :
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh?hl=en
(require 'edit-server)
(edit-server-start)

;; ediff buffers side-by-side, like it should
(setq ediff-split-window-function 'split-window-horizontally)

;; disable flyspell using this on a per-buffer basis
;; http://stackoverflow.com/a/8849228
;; (defun my-no-flyspell-mode (&optional rest)
;;   (flyspell-mode -1))

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

;; ls command from GNU coreutils (brew install...)
;; http://truongtx.me/2013/04/25/dired-as-default-file-manager-5-customize-ls-command/
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/usr/local/bin/gls")

;; ---------------------------------------------------------
;; Font customizations
;; ---------------------------------------------------------

;; font size
(set-face-attribute 'default nil :height 140)

;; turn on flyspell-mode in LaTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; make flyspell operate as per British English
(ispell-change-dictionary "english" t)

;; ido mode
(ido-mode t)
(setq
 ido-enable-flex-matching t      ; be flexible
 ido-everywhere t
 ido-confirm-unique-completion t ; wait for RET, even with unique completion
 ido-save-directory-list-file "~/.emacs.d/ido.last"
 ido-case-fold  t                ; be case-insensitive
 )

;; auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 78)

;; disable gui file dialog
(setq use-dialog-box nil)

;; allow typing y/n instead of yes/no in most cases
(fset 'yes-or-no-p 'y-or-n-p)

;;set the keybinding so that f3 will start the shell
(global-set-key [f3] 'shell)

(global-set-key [f4] 'save-close)

;; http proxy
;; (setenv "http_proxy" "http://proxy.iiit.ac.in:8080")

;; explicitly unset proxy (scrub off any that may have been inherited from the
;; environment -- sometimes gnome etc. have proxies set in weird places)
(setq url-proxy-services nil)

;; use the parent dir name to distinguish b/w buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; gpg location (mac)
;; should've got auto-set when we put /usr/local/bin prior to /usr/bin in $PATH as per the suggestion of 'brew doctor'
(setq epg-gpg-program "/usr/local/bin/gpg")
;; use gpg1, from this dir: /usr/local/opt/gnupg/libexec/gpgbin
;; (setq epg-gpg-program "/usr/local/opt/gnupg/libexec/gpgbin/gpg")
;; the above line is no longer needed, having used the solution presented here:
;; https://github.com/IJHack/qtpass/issues/156#issuecomment-224629448
;; according to which, you need to do 2 things to fix all issues:
;; brew install pinentry-mac
;; echo "pinentry-program /usr/local/bin/pinentry-mac" >> ~/.gnupg/gpg-agent.conf
;; after that you get a GUI password entry prompt that works

;; EasyPG
;; seamless .gpg file handling
(require 'epa-file)
;; (epa-file-enable) ; not needed. Happens on loading epa-file
;; (setenv "GPG_AGENT_INFO" nil) ; prevent GUI Password Prompt


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

;; ;; For maximize (NOT full screen!) on starting Emacs
;; ;; taken from Prof.Choppella's dotemacs, function renamed appropriately
;; (defun toggle-maximize ()
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
;; (when window-system
;;   (toggle-maximize))

;; ;; toggle full screen, bind to [f11]
;; ;; http://www.emacswiki.org/emacs/FullScreen#toc17
;; (defun toggle-fullscreen ()
;;   "Toggle full screen on X11"
;;   (interactive)
;;   (when (eq window-system 'x)
;;     (set-frame-parameter
;;      nil 'fullscreen
;;      (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

;; (global-set-key [f11] 'toggle-fullscreen)

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

;; ruby mode for these filetypes
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|god\\|cap\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\|Rake\\|Cap\\|Gem\\)file\\'" . ruby-mode))

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


;; opening pdf at current cursor position
;; (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
;; (setq TeX-view-program-selection '(((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open")))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)

;; Ebib BibTeX database manager
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(setq ebib-preload-bib-files
      '("~/Documents/SERL/rsys-svn/Reference Material/refs.bib"))

;; ;; markdown / git flavored markdown editing
;; (require 'markdown-mode)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; ;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; ;; apache-mode | major mode to edit apache config files
;; (require 'apache-mode)
;; (add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
;; (add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
;; (add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
;; (add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
;; (add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

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

;; turn on flyspell-mode in Org
;; (add-hook 'org-mode-hook 'flyspell-mode)

;; make org use evince for pdfs
;; (setq org-file-apps
;;       '((auto-mode . emacs)
;;         ("\\.mm\\'" . default)
;;         ("\\.x?html?\\'" . default)
;;         ("\\.pdf\\'" . evince)))

;; for latex exports
;;(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
;; to allow LaTeX_CLASS: IEEEtran
;; (require 'org-latex)
;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))
;; (unless (find "IEEEtran" org-export-latex-classes :key 'car
;;               :test 'equal)
;;   (add-to-list 'org-export-latex-classes
;;                '("IEEEtran"
;;                  "\\documentclass{IEEEtran}
;;                  [NO-DEFAULT-PACKAGES]"
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; per-file-class with minimal packages
;; (unless (find "per-file-class" org-export-latex-classes :key 'car
;;               :test 'equal)
;;   (add-to-list 'org-export-latex-classes
;;                '("per-file-class"
;;                  "\\documentclass{article}
;;                 [NO-DEFAULT-PACKAGES]
;;                 [EXTRA]"
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; enable markdown export
(require 'ox-md)

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

;; tell AUCTeX where TeX binaries live on a mac
;; (if (system-is-mac)
;;     (add-to-list 'exec-path "/Library/TeX/texbin"))
(setenv "PATH"
  (concat
   "/Library/TeX/texbin" ":" (getenv "PATH")))

;; [package.el] persistent scratch
(require 'persistent-scratch)
(persistent-scratch-setup-default)

;; [package.el] transpose-frame
(require 'transpose-frame)

;; ;; [package.el] org-jira | http://baohaojun.github.io/org-jira.html
;; (setq jiralib-url "https://bugs.indeed.com/")
;; ; you need make sure the jiralib-url is correct. Login your jira
;; ; server in browser, the home page URL should be like:
;; ; https://issues.apache.org/jira/secure/Dashboard.jspa
;; ; remove the "/secure/Dashboard.jspa" part and you get the jiralib-url:
;; ; "https://issues.apache.org/jira"

;; (require 'org-jira)
;; ; jiralib is not explicitly required, since org-jira will load it

;; ;; [ELPA] smooth-scrolling | http://www.adamspiers.org/elisp/smooth-scrolling.el
;; (require 'smooth-scrolling)
;; (setq smooth-scroll-margin 6)

;; [package.el] openwith
(require 'openwith)
(setq openwith-associations
      '(
        ("\\.\\(dmg\\|pdf\\|ps\\|mp3\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx\\|nfo\\|torrent\\|flv\\|avi\\|mp4\\|mkv\\|mpg\\|mpeg\\|vob\\|divx\\|jpg\\|jpeg\\|png\\|gif\\|svg\\)\\'" "open" (file))
        ))
(openwith-mode t)

;; python version management


;; elpy for python dev
;; installed using package.el (M-x package-list-packages)
;; ;; mac => sudo easy_install pip
;; ;;        install python packages into system python as listed on elpy page
(elpy-enable)

;; fix python interpreter incompatible with readline issue
;; https://github.com/gregsexton/ob-ipython/issues/28#issuecomment-182346001
(setq python-shell-completion-native-enable nil)

;; puppet related config
;; indent with 4 spaces
(setq puppet-indent-level 4)
(setq puppet-include-indent 4)

;; paradox --- nicer UI for package.el | installed using package.el
(require 'paradox)
(paradox-enable)

;; column highlighting in yaml files
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)

;; (require 'openwith)
;; (setq openwith-associations
;;       '(
;;         ("\\.pdf\\'"  "evince"  (file))
;;         ("\\.ps\\'"   "evince"  (file))
;;         ("\\.mp3\\'"  "clementine"  (file))
;;         ("\\.doc\\'"  "libreoffice" (file))
;;         ("\\.docx\\'" "libreoffice" (file))
;;         ("\\.xls\\'"  "libreoffice" (file))
;;         ("\\.xlsx\\'" "libreoffice" (file))
;;         ("\\.ppt\\'"  "libreoffice" (file))
;;         ("\\.pptx\\'" "libreoffice" (file))
;;         ("\\.nfo\\'"  "nfoview" (file))
;;         ("\\.torrent\\'"  "qbittorrent" (file))
;;         ("\\.flv\\'"  "vlc" (file))
;;         ("\\.avi\\'"  "vlc" (file))
;;         ("\\.mp4\\'"  "vlc" (file))
;;         ("\\.mkv\\'"  "vlc" (file))
;;         ("\\.mpg\\'"  "vlc" (file))
;;         ("\\.mpeg\\'"  "vlc" (file))
;;         ("\\.vob\\'"  "vlc" (file))
;;         ("\\.divx\\'"  "vlc" (file))
;;         ("\\.jpg\\'"  "eom" (file))
;;         ("\\.jpeg\\'"  "eom" (file))
;;         ("\\.png\\'"  "eom" (file))
;;         ("\\.gif\\'"  "eom" (file))
;;         ("\\.svg\\'"  "inkscape" (file))
;;         ;; ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "vlc" ("-idx" file))
;;         ;; ("\\.\\(?:jp?g\\|png\\)\\'" "eom" (file))
;;         ))
;; (openwith-mode t)

;; emacsclient EDITOR for magit
(setenv "EDITOR" "emacsclient")

;; more PATH tweaking
(add-to-list 'exec-path "/usr/local/bin")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (paradox yaml-tomato json-snatcher puppet-mode caps-lock elpy web-beautify unison-mode tidy ssh-config-mode smooth-scrolling smart-compile shell-history shell-current-directory shell-command rust-mode rake racket-mode python-mode pyenv-mode persistent-scratch org-octopress org-jira openwith markdown-mode+ magit-tramp magit-gh-pulls launchctl launch jist jira-markup-mode inf-ruby edit-server csv-mode auto-complete auctex ansible-doc ansible))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
