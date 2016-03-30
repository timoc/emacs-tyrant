;; -*- mode: emacs-Lisp; outline-minor-mode:t; tab-width:3; mode:auto-revert; -*-
;; Copyright (C) 1989-2016  Tim O'Callaghan
;;
;; This file is free software licensed under the terms of the
;; MIT License.
;;

;; Emacs Tyrant.
;; (or with great power comes great configurability responsibility)
;;
;; To bring the Tyrany of the default to the emacs-masses.
;;
;; Before I get into it, its best that i lay bare the inconvienient
;; truth of using a computer for anything. It is best described by
;; Douglas Adams.
;;
;; "The History of every major Galactic Civilization tends to pass
;; through three distinct and recognizable phases, those of:
;;
;; Survival, Inquiry and Sophistication
;;
;; otherwise known as the How, Why, and Where phases. For instance,
;; the first phase is characterized by the question 'How can we eat?'
;; the second by the question 'Why do we eat?' and the third by the
;; question 'Where shall we have lunch?”
;; - Douglas Adams, The Restaurant at the End of the Universe

;;
;; The Tyrant is here to pull your emacs configuration kicking and
;; screaming to the sophistication stage. Rather than try to figure
;; out how to do something, the tyrant will give you the workflows to
;; start getting things done.
;;
;; The whole concept is based on a single shared emacs config that can work on
;; multiple hosts and have local and shared configs using cloud storage. Using
;; its bootstrapping it will sync your configuration across devices.
;; NOTE: must have -*- mode:auto-revert -*- for bootstrapping to work properly

;; This is built upon:

;; this init file - which will bootstrap your whole local installation
;; org-mode + org babel - to document the workflow
;; el-get + git - to mangage the packages

;;
;; other tools:
;; dropbox - used to syncrhoise changes across installations

;; use:
;; save this file as your init.el, create a README.org literate init.el
;; update the ze variable to point to the appropriate place.
;; start emacs

;; to make maitainance easier, this sets up f9 as the faskey prefix for aching
;; this file, your org file, and the untangled org files.

;; note this is not for the timid yet. It is/was a work in progress. I've posted
;; this as a gist, so others can use it to create their own lierate emacs. I'm
;; currently looking to integrate this workflow + org mode approach into
;; spacemace for Space tyrant!.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note on my function and variable definitions.
;; Zaphod who was/is? president of the galaxy, is the current tyrant, so to
;; avoid namespace clashes:
;;
;; ze:     zemacs tyrant configuration variable prefix
;; ze@     zemacs tyrant function prefix


;; map my speed key (meta :) for eval elisp region, and (meta-?) for help
(define-key global-map [(meta :)] 'eval-region)
(define-key global-map [(meta \?)] 'help-command)

;; f9 , shift f9 etc is the fastkey to fast fix configure problems
;; can hit f9 + f1 for the f9 key-binding menu
;; f9 - emacs and org config file fast load - can hit f9-f1 for config file menu
(define-prefix-command 'ze@emacs-tangled-config 'ze:emacs-tangled-config-map)
(define-key global-map [(f9)] ze:emacs-tangled-config-map)

;; add extra doc support for babel and lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-path. the worst thing before CLASSPATH was created to out-do it.
;; This manages paths
(defun ze@add-to-load-path (this-directory &optional with-subdirs recursive prepend)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
   Add all its subdirectories not starting with a '.' if the
   optional argument WITH-SUBDIRS is not nil.
   Do it recursively if the third argument is not nil.
   Prepend to list instead if prepend"
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with `~')
      (while (not (string= this-directory (expand-file-name this-directory)))
        (setq this-directory (expand-file-name this-directory)))
      (cond ((directory-files this-directory t "^.*\\.el[c]?")
             (cond (prepend
                    (message "=- Prepending `%s' to load-path..." this-directory)
                    (add-to-list 'load-path this-directory))
               (t (message "=+ Appending `%s' to load-path..." this-directory)
						(add-to-list 'load-path this-directory t))
               )))
      ; remove duplicates if they exist
      (delq nil (delete-dups load-path))

      (when with-subdirs
        (while files
          (setq dir-or-file (car files))
          (when (file-directory-p dir-or-file)
            (if recursive
                (ze@add-to-load-path dir-or-file with-subdirs recursive prepend)
              (ze@add-to-load-path dir-or-file)))
          (setq files (cdr files)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ze:add-dir-to-env; utility function to add paths to path based
;; environmental variables, in a cross platform way, while also
;; testing for duplicates
(defun ze@add-dir-to-env (env-var dirlist &optional front &optional full)
  (dolist (dir dirlist)
    ;; (setq path-var ())
    (cond ((stringp dir)
           (setq env (getenv env-var))
           (setq edir (expand-file-name dir))
           (message "== Testing PATH:= %s - (expanded=%s)" dir edir)
           (cond ((file-directory-p edir)
                  ;; (add-to-list 'path-var edir front)
                  (message "== Searching unexpanded PATH:= %s <--> %s" (regexp-quote dir) env)
                  (unless (and (string-match (regexp-quote dir) env)
                               (string-match (regexp-quote edir) env))
                    (message "== Adding PATH:= %s\n" edir)
                    (if front
                        (setenv env-var (concat edir path-separator env))
                      (setenv env-var (concat env path-separator edir))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to untangle the tangled.org org-mode emacs-lisp tangled
;; config file, to an untangled.el file. If the tangled source does
;; not exist or, if the source is older than the generated .el file,
;; then load the .el if it exists.
;;
;; NOTE: all babel default header arguments are defined here
;; (org-babel-default-header-args)
;; Depending on your org-version they may default to :tangle yes or :tangle no.
;; best to explicitly define the :tangle yes and no in the #+BEGIN_SRC line.
;;
(defun ze@load-tangled-config (log-workflow-type untangled &optional tangled)
  ;; calculate untangled version
  (if (and (stringp tangled) (stringp untangled) (file-exists-p tangled) (featurep 'ob-tangle))
      (progn
        (let* ((unctime (nth 6 (file-attributes untangled)))
               (tctime (nth 6 (file-attributes tangled))))
          ;; check timestamp for untangled is newer than tangled. If not then convert
          (if (or (< (float-time unctime) (float-time tctime))
						(not (file-exists-p untangled)))
              (progn
                (message (concat "++ untangling required- " (format-time-string "%Y-%m-%d %T" tctime) " newer than " (format-time-string "%Y-%m-%d %T" unctime)))
                (message ">> untangling config for:%s from %s into %s " log-workflow-type tangled untangled)
                (org-babel-tangle-file tangled untangled)
                (byte-compile-file untangled))
            (message (concat "-- no untangling required- "
                             (format-time-string "%Y-%m-%d %T" tctime)
                             " older than " (format-time-string
                                             "%Y-%m-%d %T" unctime))))
          ))
    (progn
      (if tangled
          (message "!! untangling failed for:%s from %s into %s " log-workflow-type tangled untangled))))
  (cond  ((and (stringp untangled) (file-exists-p untangled))
          (message "<< using untangled config for:%s from file:%s" log-workflow-type untangled)
          (load-file untangled))
        (t
         (message "!! could not load configuration for %s from %s or %s " log-workflow-type tangled untangled))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to bootstrap el-get if it does not exist
;;
(defun ze@bootstrap-tangled-package-managment (bootstrap-path bootstrap-url)
  ;; calculate untangled version
  ;; add el-get to load path before checking for its availability
  (ze@add-to-load-path bootstrap-path t nil)
  (unless (require 'el-get nil 'noerror)
    ;; if not available bootstrap load it from github
    (progn
      (message "!! WARNING synchronously bootstrapping el-get!!")
      (message (concat "++ bootstrapping el-get from:" bootstrap-url " into " bootstrap-path))
		(let ((download-buffer (url-retrieve-synchronously bootstrap-url)))
		  (save-excursion
			 (set-buffer download-buffer)
			 ;; we may have to trim the http response
			 (goto-char (point-min))
			 (end-of-buffer)
			 (eval-print-last-sexp)))))
  ;; add el-get to load path again
  (ze@add-to-load-path bootstrap-path t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Begin
;; set debug on error
(setq debug-on-error t)

;; set  default temp directories
(setq   temp-directory (expand-file-name "~/tmp/"))
(setenv "TMP" (expand-file-name "~/tmp/"))
(setenv "TMPDIR" (expand-file-name "~/tmp/"))

(message "----------------------------------------------------------------------")
(if debug-on-error (message "== debug on error"))
(defconst ze:emacs-flavour (version))
(message "++ Emacs Flavour:%s" ze:emacs-flavour)

;; set base-defaults to:
(setq make-backup-files nil)        ; stop creating those backup~ files
(setq auto-save-default nil)        ; stop creating those #auto-save# files
(setq auto-save-list-file-name nil) 
(fset 'yes-or-no-p 'y-or-n-p)       ; y/n not yes/no
(setq global-auto-revert-mode t)    ; stop asking about changed local file
(setq message-log-max t)           ; set size of message log to maximum

; setup inital fonts - readable on high dpi monitors
(set-default-font "monospace-10")
(set-frame-font   "monospace-10")

;; determine os_type;
(defconst ze:win32-p (memq system-type '(ms-dos windows-nt cygwin)))
(defconst ze:linux-p (or (eq system-type 'gnu/linux)(eq system-type 'linux)))
(defconst ze:console-p (eq (symbol-value 'window-system) nil))

;; hostname; set a postfix on tolower(HOSTNAME) with os type (-w win* -u *nix)
(defconst ze:hostname (downcase
                        (cond (ze:win32-p
                              (concat system-name "-w" ))
                             ((not ze:win32-p)
                              (concat system-name "-u" )))))

;; id-tag; 'user@hostname-type'; used for machine-specific configuration,
;; as part of machine-specific configuration files
(defconst ze:id-tag (concat (user-login-name) "@" ze:hostname))

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; roots; paths to roots folders

;; Seems silly/overkill, but bitter experience has shown it to be
;; worth it. It is much easier to have all these things encoded in one
;; place, so that if i change them, i do not need to change them
;; anywhere else.

;; they are deliberately not defined in the tangled file, because they
;; are all constants.

(defconst ze:home-dir (expand-file-name "~/"))
(defconst ze:tmpdir   (expand-file-name "~/tmp/"))
(defconst ze:usr-dir  (expand-file-name "~/usr/"))

;; set local machine root to public emacs configuration root, and
;; site-lisp(s). This is where all of the locally stored stuff will live
(defconst ze:pub-dir  (expand-file-name (concat ze:home-dir "/.dotfiles/zemacsen/")))
;; make sure pub folder exists.
(make-directory ze:pub-dir t)
;; set zemacs machine local root
(setq user-emacs-directory ze:pub-dir)
;; set cache folder in root
; (setq user-cache-directory (expand-file-name (concat ze:pub-dir "/cache/")))

;; Boxed directoriy settings
(defconst ze:box-dir (expand-file-name (concat ze:usr-dir "/box/")))
;; use mounted dropbox folder for config
;; (defconst ze:dot-dir (expand-file-name (concat ze:box-dir "/dotfiles/zemacsen/")))

;; test with to local .emacs.d
(defconst ze:dot-dir (expand-file-name (concat ze:home-dir "/.emacs.d/")))

;; usr local directory settings
(defconst ze:local-dir     (expand-file-name (concat ze:usr-dir "/local/")))
(defconst ze:local-dot-dir (expand-file-name (concat ze:local-dir "/dotfiles/zemacsen/")))

;; set local site-lisp directories
(defconst ze:pub-lisp-dir   (expand-file-name (concat ze:pub-dir "/site-lisp/")))
(ze@add-to-load-path ze:pub-lisp-dir t t)
(defconst ze:local-lisp-dir (expand-file-name (concat ze:local-dir "/site-lisp/")))
(ze@add-to-load-path ze:local-lisp-dir t t)
(defconst ze:box-lisp-dir   (expand-file-name (concat ze:dot-dir "/site-lisp/")))
(ze@add-to-load-path ze:box-lisp-dir t t)

;; set local untangled files destination
(defconst ze:untangled-lisp-dir (expand-file-name (concat ze:pub-dir "untangled/")))
;; ensure untangled folder exists
(make-directory ze:untangled-lisp-dir t)

;; this file
(defconst ze:init-conf      (expand-file-name (concat ze:home-dir "init.el")))

;; local machine conf fastkey
(defun ze@load-init-conf-bootstrapper   () "" (interactive) (find-file ze:init-conf))
(define-key ze:emacs-tangled-config-map [(?b)] 'ze@load-init-conf-bootstrapper)

;; reset default 'auto save' location for customisation
(defconst ze:custom-el-file (concat ze:pub-dir "custom.el"))
(setq custom-file ze:custom-el-file)
(defun ze@load-custom-el () "" (interactive) (find-file ze:custom-el-file))
(define-key ze:emacs-tangled-config-map [(?c)] 'ze@load-custom-el)

;; org tangled init.el
(defconst ze:init-tangled   (expand-file-name (concat ze:dot-dir "README.org")))
(defconst ze:init-untangled (expand-file-name (concat ze:untangled-lisp-dir "init.el")))
(defun ze@load-init-el-tangled   () "" (interactive) (find-file ze:init-tangled))
(defun ze@load-init-el-untangled () "" (interactive) (find-file ze:init-untangled))
;;  README.org - the initial org file fast-keys
(define-key ze:emacs-tangled-config-map [(f9)] 'ze@load-init-el-tangled)
(define-key ze:emacs-tangled-config-map [(?i)] 'ze@load-init-el-tangled)
(define-key ze:emacs-tangled-config-map [(?I)] 'ze@load-init-el-untangled)

;; org configuration file
(defconst ze:init-org-tangled   (expand-file-name (concat ze:dot-dir "orgrc.org")))
(defconst ze:init-org-untangled (expand-file-name (concat ze:untangled-lisp-dir "orgrc.el")))
(defun ze@load-org-rc-tangled   () "" (interactive) (find-file ze:init-org-tangled))
(defun ze@load-org-rc-untangled () "" (interactive) (find-file ze:init-org-untangled))
;; org-mode config access
(define-key ze:emacs-tangled-config-map [(f8)] 'ze@load-org-rc-tangled)
(define-key ze:emacs-tangled-config-map [(?o)] 'ze@load-org-rc-tangled)
(define-key ze:emacs-tangled-config-map [(?O)] 'ze@load-org-rc-untangled)

; public (box) and private (local) machine config paths
(defconst ze:mach-local-conf (expand-file-name (concat ze:local-dot-dir "mconf/" ze:hostname ".el")))
(defconst ze:mach-box-conf   (expand-file-name (concat ze:dot-dir "mconf/" ze:hostname ".el")))
(defun ze@load-mach-local-conf () "" (interactive) (find-file ze:mach-local-conf))
(defun ze@load-mach-box-conf   () "" (interactive) (find-file ze:mach-box-conf))
;; machine conf key access
(define-key ze:emacs-tangled-config-map [(?l)] 'ze@load-mach-local-conf)
(define-key ze:emacs-tangled-config-map [(?m)] 'ze@load-mach-box-conf)

;; It seems that a good 'collection' point is required for all of the
;; unused cruft and stuff that i do not want to throw away just
;; yet. Until i understand why it is there. It will all be tangled,
;; but never untangled.
(defconst ze:init-emacs-someday-tangled (expand-file-name (concat ze:dot-dir "1+TRYANT_BACKLOG.org")))
(defun ze@load-emacs-someday-tangled  () "" (interactive) (find-file ze:init-emacs-someday-tangled))
;; org-mode config access
(define-key ze:emacs-tangled-config-map [(?e)] 'ze@load-emacs-someday-tangled)

;; the other file i am always hacking on is my shell config, so adding
;; a convenience key to the keymap there too.
(defun ze@load-bashrc-untangled () "" (interactive) (find-file (expand-file-name "~/.bashrc")))
(define-key ze:emacs-tangled-config-map [(?B)] 'ze@load-bashrc-untangled)

;; make sure destination folders exist
(defconst ze:hist-dir              (concat ze:pub-dir "histories/"))
(defconst ze:autosave-directory    (concat ze:pub-dir "autosave"))
(defconst ze:eshell-ctrl-files-dir (expand-file-name (concat ze:pub-dir "eshell/")))
(make-directory ze:hist-dir t)
(make-directory ze:autosave-directory t)
(make-directory ze:eshell-ctrl-files-dir t)

;; history filenames
(defconst ze:desktop-file          (concat ze:id-tag "-desktop-E"))
(defconst ze:hist-file-prefix      (concat ze:hist-dir ze:id-tag))
(defconst ze:save-hist-file        (concat ze:hist-file-prefix "-savehist-E"))
(defconst ze:bookmark-file         (concat ze:hist-file-prefix "-bookmark-E.bmk"))
(defconst ze:save-places-file      (concat ze:hist-file-prefix "-places-E"))
(defconst ze:autosave-prefix       (concat ze:pub-dir "autosave/saves-"))

;; broswer to use for web links
(defconst ze:browser-binary        (expand-file-name "~/usr/local/libexec/firefox/firefox"))

(defconst ze:package-dir (expand-file-name (concat ze:pub-dir "/elpa/")))
(setq package-user-dir ze:package-dir)

(defconst ze:package-archives
  '(("melpa-stable" . "http://stable.melpa.org/packages/")
    ("ELPA" . "http://tromey.com/elpa/")
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
    ("SC" . "http://joseito.republika.pl/sunrise-commander/")
    ))

(defconst ze:el-get-dir  (expand-file-name (concat ze:pub-dir "/el-get/")))
(defconst ze:el-get-personal-recipe-dir  (expand-file-name (concat ze:dot-dir "/el-get-recipes/")))
;; (add-to-list 'el-get-recipe-path ze:el-get-personal-recipe-dir)
(defconst ze:el-get-personal-workflow-dir  (expand-file-name (concat ze:dot-dir "/el-get-workflows/")))

;; startup at home
(progn (cd ze:home-dir))

;; mix in some common lisp-ness for el-get and org
(require 'cl)
;; require find for later
(require 'find-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check its Emacs 24 and has the package manager av
(unless (>= emacs-major-version 24)
  (message "!! WARNING Emacs Version < 24 - may cause trouble"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enaure el-get and elpa are ready for use before untangling. Easier
;; to do outside of tangled congiguration
;;
;; setup url fetch debugging
(setq url-debug t)

;; setup el-get, with debugging etc.
(setq el-get-verbose t)
(setq el-get-byte-compile nil)
  ;; el-get package download to - sets root for el-get operation
(setq el-get-dir ze:el-get-dir)

;; fix for problems with HTTP async timeouts
(setq url-http-attempt-keepalives nil)

;; set remote repository url (mine/original)
;; (setq el-get-git-install-url "git@github.com:timoc/el-get.git")
;; (setq ze:el-get-bootstrap-url "https://github.com/timoc/el-get/raw/master/el-get-install.el")
(setq ze:el-get-bootstrap-url "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
(ze@bootstrap-tangled-package-managment (concat ze:el-get-dir "/el-get") ze:el-get-bootstrap-url)
;; (add-to-list 'el-get-recipe-path ze:el-get-bootstrap-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ensure org-mode is bootstrapped and emacs lisp is an active babel
;; language before anythine else

;; check el-get is installed
(unless (require 'el-get nil 'noerror)
  (message "!! WARNING package el-get is not available"))

(require 'package)
(setq package-archives ze:package-archives)
(package-initialize)

;; new el-get wrikle to get elpa/melpa working
(unless (require 'el-get-elpa nil 'noerror)
  (message "!! WARNING package el-get is not available"))

;; add personal recipe folder
(add-to-list 'el-get-recipe-path ze:el-get-personal-recipe-dir)

;; set personal workflow customisations
(setq el-get-user-package-directory ze:el-get-personal-workflow-dir)

;; bootstrap package and non-native org-mode as minimum install
(el-get 'sync (append '(package org-mode)
							 (mapcar 'el-get-source-name el-get-sources)))

;; remap CUA disputed keys (see `org-disputed-keys')
;; must be set before org-mode is loaded
(setq org-replace-disputed-keys t)

(unless (require 'org nil 'noerror)
  (message "!! WARNING package org-mode is not available"))

;; set org to the defined directory
(defconst ze:gtd-dir (expand-file-name (concat ze:home-dir "/1-ORG_GTD/")))
(defconst ze:org-dir (expand-file-name (concat ze:gtd-dir "/1-PROCESS_GTD/")))
(setq org-directory ze:org-dir)

(require 'ob-tangle)
(require 'ob-emacs-lisp)
(message "== using ob-tangle from org mode :%s" org-version)
;; set the languages i want babel to work on before invoking it
;; for untangling
(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk .t)         (C . t)        (comint . t)
	(css . t)        (ditaa . t)    (dot . t)
	(emacs-lisp . t) (eval . t)     (gnuplot . t)
	(java . t)       (js . t)       (latex . t)
	(ledger . t)     (makefile . t) (plantuml . t)
	(python . t)     (perl . t)     (ruby . t)
	(screen . t)     (shell . t)    (sql . t)
	(sqlite . t)     (table . t)
	))
(setq org-src-preserve-indentation t)

;; set native babel fontification (like mmm mode?)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
;; don't bug me about evals
(setq org-confirm-babel-evaluate nil)

;; ensure default header arguments have tangle set, and comments exported
(setq org-babel-default-header-args
		(cons '(:tangle . "yes")
				(assq-delete-all :tangle org-babel-default-header-args)))
(setq org-babel-default-header-args
		(cons '(:exports . "code")
				(assq-delete-all :exports org-babel-default-header-args)))

;; make babel results blocks uppercase to save confusion
(setq org-babel-results-keyword "RESULTS")

;; load machine configuration files (local settings etc)
(ze@load-tangled-config "Local mconf" ze:mach-local-conf)

;; load main configuration file
(ze@load-tangled-config "README - main config" ze:init-untangled ze:init-tangled)

;; load org configuration file
(ze@load-tangled-config "org mode" ze:init-org-untangled ze:init-org-tangled)

;; load private files
; (ze@load-tangled-config "private context zemacs" ze:init-org-tangled ze:init-untangled)

(message "-------------------------------- DONE --------------------------------")
(defconst ze:flavour-tested-on
  '(
	 "GNU Emacs 24.3.1 (x86_64-pc-linux-gnu, GTK+ Version 3.10.7)\n of 2014-03-07 on lamiak, modified by Debian"
	 "GNU Emacs 24.3.1 (x86_64-pc-linux-gnu, GTK+ Version 3.4.2)\n of 2013-04-14 on chindi10, modified by Debian"
	 "GNU Emacs 24.3.1 (x86_64-redhat-linux-gnu, GTK+ Version 3.8.2)\n of 2013-08-14 on buildvm-15.phx2.fedoraproject.org"
    "GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+ Version 2.18.9)"
    "GNU Emacs 24.4.1 (x86_64-pc-linux-gnu, GTK+ Version 3.14.9)\n of 2015-03-21 on kissel, modified by Debian"
    "GNU Emacs 24.4.1 (x86_64-redhat-linux-gnu, GTK+ Version 3.14.5)\n of 2014-11-19 on buildvm-03.phx2.fedoraproject.org"
    "GNU Emacs 24.5.1 (x86_64-pc-linux-gnu, GTK+ Version 3.16.6)\n of 2015-09-17 on lgw01-52, modified by Debian"
    "GNU Emacs 24.5.1 (x86_64-pc-linux-gnu, GTK+ Version 3.18.7)\n of 2016-02-17 on lgw01-45, modified by Debian"
    "GNU Emacs 24.5.1 (x86_64-redhat-linux-gnu, GTK+ Version 3.14.12)\n of 2015-05-07 on buildvm-08.phx2.fedoraproject.org"
    "GNU Emacs 24.5.1 (x86_64-redhat-linux-gnu, GTK+ Version 3.16.6)\n of 2015-09-14 on buildvm-10.phx2.fedoraproject.org"
    "GNU Emacs 24.5.1 (x86_64-redhat-linux-gnu, GTK+ Version 3.18.7)\n of 2016-02-03 on buildhw-05.phx2.fedoraproject.org"
	 ))

(if (not (member ze:emacs-flavour ze:flavour-tested-on))
    ;; output new flavour with embedded newlines etc
    (let ((print-escape-newlines t))
      (message "!! UPDATE TESTED FLAVOUR LIST !! with:%s" (prin1-to-string ze:emacs-flavour))))

;; startup at home
(progn (cd ze:home-dir))

;; turn off debug if enabled
(setq debug-on-error nil)
