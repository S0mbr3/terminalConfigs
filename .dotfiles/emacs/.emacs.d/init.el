(setq gc-cons-threshold (* 50 1000 000))
(defun ox/display-startup-time()
(message "Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
	    (float-time
	     (time-subtract after-init-time before-init-time)))
    gcs-done))

(add-hook 'emacs-startup-hook #'ox/display-startup-time)

(defconst my-project-path "~/dev")
(defconst my-font-size 250)
(defconst my-opacity 90)
(defconst my-leader-key "SPC")
(defconst my-linux-font "FiraCode Retina")
(defconst my-wsl-font "Fira Code Retina")
(defconst my-org-files '("~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Tasks.org"
			 "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/todo.org"
			 "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Habits.org"
			 "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/birthdays.org"))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;;(setq use-package-always-defer t)
(setq use-package-always-ensure t)
;;(setq use-package-compute-statistics t)
;;(setq use-package-verbose t)

;; Not if you want to move everything out of the ~/.emacs.d folder reliabily, set `user-emacs-directory` before loading the no-littering!
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)

;; no-littering doesn't set this by default so we must place auto save files in the same path as it uses for sessions

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-challenger-deep t)
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom(doom-modeline-height 8))

(setq inhibit-startup-message t ; Don't show the spalsh screen
      ring-bell-function 'ignore
      visible-bell nil)  ; Stop screen to flash when the bell rings

;; Turn off some uneeded ui elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1) ;; Disable tooltips
(set-fringe-mode 10) ;; give some breathing room

;;(set-frame-parameter nil 'alpha-background 70) ; For current frame
;;(add-to-list 'default-frame-alist '(alpha-background . 70)) ; For all new frames henceforth

(column-number-mode)
(global-display-line-numbers-mode 1) ;Show line numbers
(setq display-line-numbers-type 'relative)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		vterm-mode-hook
		treemacs-mode-hook
		compilation-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-parameter nil 'alpha-background my-opacity) ; For current frame
(add-to-list 'default-frame-alist `(alpha-background . ,my-opacity)) ; For all new frames henceforth

;;(load-theme 'deeper-blue t)

;; Set font
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font my-linux-font :height my-font-size)
  (set-face-attribute 'default nil :font my-wsl-font :height my-font-size))
;;(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140)

(set-frame-parameter nil 'alpha-background my-opacity) ; For current frame
(add-to-list 'default-frame-alist `(alpha-background . ,my-opacity)) ; For all new frames henceforth

;;(load-theme 'deeper-blue t)

;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(recentf-mode 1) ;; Enable the recent file mode to select with a number recent files
(save-place-mode 1) ;; set cursor at last location known when visiting a file
(savehist-mode 1)

;; Nove customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Rever buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Mode to log commands use clm/open-command-log-buffer to see them
(use-package command-log-mode
:commands command-log-mode)
;; install all the icons
(use-package all-the-icons)

;; make unique colors for each parentheses pair to see better delimitation
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Better keybinding management 
(use-package general
  :after which-key
  :config
  (general-define-key
   "C-c C-v" 'compile-and-execute-c-code
   "C-c m" 'compile-or-recompile
   "C-c C-b" 'switch-to-previous-buffer
   "M-o" 'multi-vterm-dedicated-toggle
   "<escape>" 'keyboard-escape-quit	; Make escape key quit prompts
   "C-x b" 'counsel-switch-buffer
   "C-M-J" 'counsel-load-theme
   "C-s" 'counsel-grep-or-swiper)
  ;;(defconst my-leader "C-SPC")
  ;; Creating a leader key
  (defconst my-global-leader "C-SPC")
  (defconst my-leader "SPC")
  (general-create-definer ox/leader-keys
    :keymaps '(normal insert visual emacs)
    ;;:keymaps '(normal)
    :prefix my-leader
    :global-prefix my-global-leader)
  (ox/leader-keys
    "r" '(restart-emacs :which-key "restart")

    ";" '(comment-or-uncomment-region :which-key "comment or uncomment region")

    "ff" '(find-file :which-key "find-file")
    "ff" '(find-file :which-key "find-file")
    "fr" '(recentf-open-files :which-key "Recent opened files")
    "ft" '(treemacs-select-window :which-key "Open treemacs")

    "c" '(:ignore t :which-key "compiling")
    "cc" '(compile :which-key "compile")
    "cr" '(recompile :which-key "recompile")

    "t" '(:ignore t  :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package which-key
   :after evil
  ;;:defer 0
  ;;:init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))
;; To allow M-x to be sorted from most recent used 
(use-package smex
  :after ivy
  :config
  (smex-initialize))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package harpoon
  :after (general which-key)
  :config
  (ox/leader-keys
    ;;"h" '(:ignore t :which-key "Org")
    "0" '(harpoon-add-file :whick-key "Add file to Harpoon")
    "1" '(harpoon-go-to-1 :which-key "harpoon file 1")
    "2" '(harpoon-go-to-1 :which-key "harpoon file 2")
    "3" '(harpoon-go-to-1 :which-key "harpoon file 3")
    "4" '(harpoon-go-to-1 :which-key "harpoon file 4")
    "5" '(harpoon-go-to-1 :which-key "harpoon file 5")
    "6" '(harpoon-go-to-1 :which-key "harpoon file 6")
    "7" '(harpoon-go-to-1 :which-key "harpoon file 7")
    "8" '(harpoon-go-to-1 :which-key "harpoon file 8")
    "9" '(harpoon-go-to-1 :which-key "harpoon file 9")))

(use-package hydra
:after (general which-key)
:defer t
:config
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(ox/leader-keys
  "h" '(:ignore t :which-key "hydra")
  "hs" '(hydra-text-scale/body :which-key "scale text")))

(defun kill-current-buffer-without-confirm ()
  "Kill the current buffer without confirmation."
  (interactive)
  (let (kill-buffer-query-functions) ; Disable confirmation
    (kill-buffer (current-buffer))))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Better help view and features
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package term
:defer 0
:config
(setq explicit-shell-file-name "zsh"))
;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package vterm
  :defer 0
  :after (general which-key)
  :config
  (setq vterm-max-scrollback 10000)
  (setq term-prompt-regexp "^[^❯\n]*[❯] *"))
;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))
;; :hook (vterm-mode . (lambda ()
;; 			(evil-emacs-state))))
(use-package multi-vterm
  :after vterm
  ;; :after vterm
  ;; :hook (vterm-mode . (lambda ()
  ;; 			(evil-emacs-state))))
  :config
  (ox/leader-keys
    "s" '(:ignore t :which-key "shells")
    "sv" '(multi-vterm :which-key "new multi-vterm buffer")
    "so" '(multi-vterm-dedicated-toggle :which-key "toggle multi-vterm")
    "sp" '(multi-vterm-prev :which-key "multi-vterm prev")
    "sn" '(multi-vterm-next :which-key "multi-vterm next")
    "se" '(eshell :whick-key "eshell"))
  (setq multi-vterm-dedicated-window-height-percent 40))

(if (eq system-type 'gnu/linux)
	(setq explicit-shell-file-name "zsh")
    (setq explicit-shell-file-name "powershell.exe")
    (setq explicit-powershel.exe-args'()))

(use-package eshell-git-prompt
  :after eshell)

(defun ox/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)

  (setq eshell-history-size 10000
	eshell-buffer-maximun-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . ox/configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'multiline)

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

;; Dependencies for evil mode undo features
(use-package undo-tree)
(use-package undo-fu)
;; For evil g; g, motions and last-change-register "."
(use-package goto-chg)

;;hook to start modes without evil mode
(defun ox/evil-hook ()
  (message "ox/evil-hook was called") ; add this line
  (dolist (mode '(Custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  vterm-mode
		  term-mode
		  ))
    (add-to-list 'evil-emacs-state-modes mode)))
;;(evil-set-initial-state mode 'emacs)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :hook (evil-mode . ox/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  ;;(evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'normal)

(defun print-evil-state ()
  "Print the value of evil-emacs-state-modes."
  (interactive)
  (prin1 evil-emacs-state-modes))
(ox/leader-keys
"e" '(:ignore t :which-key "Evil")
"eu" '(evil-collection-unimpaired-move-text-up :which-key "Evil")
  "ep" '(print-evil-state :which-key "print evil state")
"ed" '(evil-collection-unimpaired-move-text-down :which-key "Evil"))
(defhydra hydra-move-text (:timeout 4)
  "scale text"
  ("j" evil-collection-unimpaired-move-text-up "Move up")
  ("k" evil-collection-unimpaired-move-text-down "Move down")
  ("f" nil "finished" :exit t))
(ox/leader-keys
  "h" '(:ignore t :which-key "hydra")
  "hm" '(hydra-move-text/body :which-key "Move text")))

(evil-mode 1)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-numbers
  :after evil
  :config
  (general-define-key
   :states 'visual
   "g C-a" 'evil-numbers/inc-at-pt-incremental
   "g C-x" 'evil-numbers/dec-at-pt-incremental)
  (ox/leader-keys
    "i" '(:ignore t :which-key "increment")
    "ia" '(evil-numbers/inc-at-pt :which-key "Imcrement")
    "ix" '(evil-numbers/dec-at-pt :which-key "Decrement")))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p my-project-path)
    (setq projectile-project-search-path `(,my-project-path)))
  (setq projectile-switch-projection-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package lua-mode
  :mode "\\.lua\\'")
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package flycheck
  :after lsp-mode
  :ensure t
  :init (global-flycheck-mode))

(defun ox/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :hook
 ((lsp-mode . ox/lsp-mode-setup)
    (c-mode . lsp-deferred)
    (python-mode . lsp-deferred)
    (lua-mode . lsp-deferred)
    (typescript-mode . lsp-deferred)
    (js-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  ;; The path to lsp-mode needs to be added to load-path as well as the
  ;; path to the `clients' subdirectory.
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
:after lsp-mode
:commands lsp-ivy-workspace-symbol
:hook (lsp-mode . lsp-ui-mode)
:custom(lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
:after lsp-mode
:commands lsp-treemacs-errors-list
:config
(lsp-treemacs-sync-mode t))
(use-package treemacs-evil
:after lsp-treemacs)
(use-package treemacs-projectile
:after lsp-treemacs)

(use-package lsp-ivy
:after lsp-mode)

(use-package company
:after lsp-mode
:hook ((prog-mode . company-mode)
             (lisp-interaction-mode . company-mode))
:bind (:map company-active-map
("<tab" . company-complete-selection))
(:map lsp-mode-map
("<tab>" . company-indent-or-complete-common))
:custom
(company-minimum-prefix-length 1)
(company-idle-delay 0.0))

(use-package company-box
:hook (company-mode . company-box-mode))

;; We are making magit getting the full buffer size
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Allow to work with forges to get informations about repositories (notifications, issues, pull requests etc)
(use-package forge
:after magit)

(defun ox/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . ox/org-mode-setup)
  :config
  (message "hi from org-mode")
  (setq org-ellipsis " ⮧"
	org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files my-org-files)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)
  ;;(setq python-indent-offset 4) ; Set indentation to 4 spaces (or any other desired value)


  (require 'org-indent)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
	'(("Archive.org" :maxlevel . 1)
	  ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
           (file+olp+datetree "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
	  ("jm" "Meeting" entry
           (file+olp+datetree "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

	  ("w" "Workflows")
	  ("we" "Checking Email" entry (file+olp+datetree "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line (file+headline "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
	      (lambda () (interactive) (org-capture nil "jj")))
  (ox/leader-keys
    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Open org-agenda")
    "ot" '(org-todo-list :which-key "Open all TODO lists")
    "oc" '(org-capture :which-key "Open org-capture")))


(use-package org-superstar
  :after org
  :config
  ;;(setq org-superstar-hide-leading-stars t)
  (setq org-superstar-leading-bullet " ")
  ;; Hide away leading stars on terminal.
  (setq org-superstar-leading-fallback ?\s))
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-superstar-mode 1)))

;; Center the text, and set a max column width to go next line in org mode
(defun ox/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ox/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (shell . t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil)
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell :results output"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("cc" . "src C")))

;; Automatically tangle our Emacs.org config file when we save it
(defun ox/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

    (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ox/org-babel-tangle-config)))

;;(require 'treesit)
;;(setq treesit-extra-load-path '("/usr/local/lib"))
;;
;;  
;;  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
;;  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
;;  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
;;  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
;;  (push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
;;  (push '(c-mode . c-ts-mode) major-mode-remap-alist)
;;  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
(use-package tree-sitter-langs
:defer 0)
(use-package tree-sitter
:after tree-sitter-langs
:config
;; Activate tree-sitter globally (minor mode registered on every buffer)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(unless (package-installed-p 'posframe)
  (package-refresh-contents)
  (package-install 'posframe))


(defvar c-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'quit-c-posframe)
    map)
  "Keymap for `c-popup-mode'.")

(define-minor-mode c-popup-mode
  "Minor mode to quit the c popup"
  :init-value nil
  :lighter " C-Popup"
  :keymap c-popup-mode-map
  :global t
  (if c-popup-mode
      (message "C popup mode enabled")
    (message "C popup mode disabled")))

(defun compile-and-execute-c-code ()
  "Save, compile, and execute C code, showing the result in a posframe."
  (interactive)
  ;; Check if c-popup-mode is already on.
  (when c-popup-mode
    ;; If it is, turn it off.
    (c-popup-mode -1))
  (let* ((temp-file "/tmp/input.c"))
    (write-buffer-to-file (current-buffer) temp-file)
    (let* ((result (execute-c-code temp-file))
           (output-buffer (get-buffer-create "*c-output*")))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert result))
      (let ((frame (posframe-show output-buffer
                                  :position (point)
                                  :font (face-attribute 'default :font)
                                  :string nil
                                  :background-color (face-attribute 'default :background nil t)
                                  :foreground-color (face-attribute 'default :foreground nil t)
                                  :internal-border-color "black"
                                  :left-fringe 0
                                  :right-fringe 0
                                  :min-width 40
                                  :min-height 10
                                  :internal-border-width 1
                                  :border-width 1
                                  :override-parameters '((cursor-type . nil)))))
        ;; Manually set focus to the posframe.
        (select-frame-set-input-focus frame)
        (c-popup-mode 1)))))

(defun quit-c-posframe ()
  "Delete all posframes and exit the c-popup-mode."
  (interactive)
  (posframe-delete-all)
  (c-popup-mode -1))

(defun execute-c-code (temp-file)
  "Compile and execute the C code in temp-file, and return the output as a string."
  (with-temp-buffer
    (call-process-shell-command (concat "gcc -o /tmp/output " temp-file " && /tmp/output") nil t)
    (buffer-string)))

;;(global-set-key (kbd "C-c C-v") 'compile-and-execute-c-code)


;;(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)
(defun compile-or-recompile ()
  (interactive)
  (if (get-buffer "*compilation*")
      (recompile)
    (call-interactively 'compile)))

;;(global-set-key (kbd "C-c m") 'compile-or-recompile)
;;Change the size of the compilation height window to be 30%
(setq compilation-window-height (round (* 0.3 (frame-height))))
;; add a hook to adjust the height of the compilation window when the window change size

;;(defun adjust-compilation-window-height ()
  ;;(setq compilation-window-height (round (* 0.3 (frame-height)))))

;;(add-hook 'window-size-change-functions 'adjust-compilation-window-height)

;; kill current buffer without the annoying confirmation message

;; This package allow single buffer navigation in Dired
;; like (dired-kill-when-opening-new-dired-buffer t) does
;; (use-package dired-single
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "h" 'dired-single-up-directory
;;     "l" 'dired-single-buffer))
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-open
  :after dired
  ;;:commands (dired dired-jump)
  :config
  ;; Strange behaviors not picking always the good program automatically
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
				("mkv" . "mpv"))))

;; When using compile or recompile command if there is some colord characters
;; it does not format well I had to use ansi-color with a hook in compilation mode
(require 'ansi-color)

(defun my-ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer)

(use-package auto-package-update
  :defer 0
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Set PowerShell as default shell
;; (setq explicit-shell-file-name "C:/Program Files/PowerShell/7-preview/pw;; sh.exe")
;;(setq shell-file-name "C:/Program Files/PowerShell/7-preview/pwsh.exe")
;;(setq explicit-pwsh.exe-args '("-NoLogo" "-NonInteractive"))
;;(setenv "SHELL" shell-file-name)
;;(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;;(use-package powershell
  ;;  :config
    ;; Change default compile command for powershell
    ;;(add-hook 'powershell-mode-hook
    ;;(lambda ()
      ;;(set (make-local-variable 'compile-command)
	;;   (format "powershell.exe -NoLogo -NonInteractive -Command \"& '%s'\""             (buffer-file-name))))))
;; Set PowerShell as default shell
;;(setq explicit-shell-file-name "C:/Program Files/PowerShell/7-preview/pwsh.exe")
;;(setq shell-file-name explicit-shell-file-name)
;;(add-to-list 'exec-path "C:/Program Files/PowerShell/7-preview/pwsh.exe")
;;(add-to-list 'exec-path "C:/Users/benja/Documents/PowerShell/Modules")
;;(add-to-list 'exec-path "C:/Program Files/PowerShell/Modules")
;;(add-to-list 'exec-path "c:program files/powershell/7-preview/Modules")
;;(add-to-list 'exec-path "C:/Program Files (x86)/WindowsPowerShell/Modules")
;;(add-to-list 'exec-path "C:/Windows/system32/WindowsPowerShell/v1.0/Modules")
;;(add-to-list 'exec-path "C:/Program Files (x86)/AutoIt3/AutoItX")
;;(global-set-key (kbd "M-o") 'multi-vterm-dedicated-toggle)
;;(global-set-key (kbd "C-f10") 'vterm-toggle)
;;:load-path "C:/Users/benja/builds/emacs-libvterm/")




;;(setq explicit-shell-file-name "C:/Program Files/PowerShell/7-preview/pwsh.exe")
;k(setq shell-file-name explicit-shell-file-name)
;;
;;(add-to-list 'exec-path "C:/Users/benja/AppData/Local/Programs/oh-my-posh/bin/")
;;(defun my-powershell ()
 ;; "Open a new shell buffer with PowerShell in interactive mode."
  ;;(interactive)
  ;;(let ((explicit-shell-args '("-NoExit" "-Command" "Set-Location C:\\Users\\YourUserName")) ; replace with your username
  ;;      (explicit-shell-file-name "C:/Program Files/PowerShell/7-preview/pwsh.exe"))
   ;; (call-interactively #'shell)))
;; To be able to use arrow key with comint-mode hook (for powershell)
;;(add-hook 'comint-mode-hook
  ;;        (lambda ()
    ;;        (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
      ;;      (define-key comint-mode-map (kbd "<down>") 'comint-next-input)))

;; Make gc pauses faster by decreasubg tge threshold.
(setq gc-cons-threshold (* 2 1000 000))