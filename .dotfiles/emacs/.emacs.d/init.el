;;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1000 000))
(defun ox/display-startup-time()
(message "Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
	    (float-time
	     (time-subtract after-init-time before-init-time)))
    gcs-done))

(add-hook 'emacs-startup-hook #'ox/display-startup-time)

(setq my-project-path "~/dev")
(setq my-font-size 200)
(setq my-opacity 90)
(setq my-leader-key "SPC")
;;(setq my-linux-font "FiraCode Retina")
;;(setq my-linux-font "Ubuntu Mono")
;;(setq my-wsl-font "Fira Code Retina")
;; (setq my-linux-font "0xProto Nerd Font")
;; (setq my-windows-font "0xProto Nerd Font")

(setq my-linux-font "CaskaydiaCove Nerd Font")
(setq my-windows-font "CaskaydiaCove Nerd Font")

(setq my-org-files '("~/syncthing/Sync/org-files/Tasks.org"
			 "~/syncthing/Sync/org-files/todo.org"
			 "~/syncthing/Sync/org-files/Habits.org"
			 "~/syncthing/Sync/org-files/Shopping.org"
			 "~/syncthing/Sync/org-files/Journal.org"
			 "~/syncthing/Sync/org-files/birthdays.org"))

(setq ox/enable-ivy nil )
(setq ox/enable-vertico t)
(setq ox/enable-cape t )

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;(setq use-package-always-defer t)
;;(setq use-package-always-ensure nil)
(setq package-enable-at-startup nil)
(setq package-native-compile t)
;;(setq use-package-compute-statistics t)
;;(setq use-package-verbose t)

;; Not if you want to move everything out of the ~/.emacs.d folder reliabily, set `user-emacs-directory` before loading the no-littering!
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
:straight t)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
;; no-littering doesn't set this by default so we must place auto save files in the same path as it uses for sessions

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (use-package doom-themes
    :straight t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;;(load-theme 'doom-challenger-deep t)
    ;;(load-theme 'doom-moonlight t)
    (load-theme 'doom-outrun-electric t)

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
  :straight t
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

(use-package ligature
:straight t
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

  (use-package eaf
    :disabled t
    :straight nil
    :load-path "~/.cache/emacs/site-lisp/emacs-application-framework"
    :custom
					  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
    (eaf-browser-continue-where-left-off t)
    (eaf-browser-enable-adblocker t)
    (browse-url-browser-function 'eaf-open-browser)
    (eaf-browser-auto-import-chrome-cookies t)
    :config
    (defalias 'browse-web #'eaf-open-browser)
    ;;(eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
    ;;(eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
    ;;(eaf-bind-key take_photo "p" eaf-camera-keybinding)
    ;;(eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
  ;;(setq eaf-webengine-pc-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36")
  (setq eaf-webengine-pc-user-agent "Mozilla/5.0 (X11; Linux i686; rv:109.0) Gecko/20100101 Firefox/118.0"))
  ;;(global-unset-key (kbd "<f1>"))
  ;;(define-key eaf-mode-map (kbd "<f1>") #'eaf-send-key)


  ;;(require 'eaf-pyqterminal)
  ;;(require 'eaf-browser)
  ;;(require 'eaf-pdf-viewer)

(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :bind ("C-c q" . chatgpt-query))

  (use-package persp-mode
    :straight t
    :defer t
    ;;:hook (persp-mode-hook . my-update-dynamic-persps)
    :init
    (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))
    ;;(add-hook 'persp-mode-hook 'my-update-dynamic-persps)
    :config


    (defun consult-persp-buffer ()
      "Switch to a buffer within the current perspective using consult."
      (interactive)
      (let* ((persp-buffers (mapcar #'buffer-name (persp-buffer-list-restricted)))
	     (buffer (consult--read persp-buffers
				    :prompt "Switch to buffer (current perspective): "
				    :sort t
				    :require-match t
				    :category 'buffer
				    :state (consult--buffer-state))))
	(switch-to-buffer buffer)))

    (global-set-key (kbd "C-x b") 'consult-persp-buffer)

    ;; Add vterm buffers to the current perspective when starting them
    ;; Automatically add buffers to current perspective when their major mode changes
    (setq persp-add-buffer-on-after-change-major-mode t)

    (defun my-persp-buffer-filter (buf)
      "Filter out buffers that start with an asterisk, except for vterm buffers."
      (let ((buf-name (buffer-name buf)))
	(not (or (and (string-prefix-p "*" buf-name)
		      (string-prefix-p "*vterm" buf-name))))))

    ;; Add the custom filter function
    (add-hook 'persp-common-buffer-filter-functions #'my-persp-buffer-filter)

    ;; Making harpoon maintaining a seperates set of bookmarks to each perspective
    (defun harpoon--file-name ()
      "File name for harpoon on current project."
      (let ((persp-name (if (and (boundp 'persp-mode) persp-mode)
			    (safe-persp-name (get-current-persp))
			  "none")))
	(concat harpoon-cache-file persp-name "_" (harpoon--cache-key))))

    (defun ox/find-first-vterm-in-persp ()
      "Find the first *vterminal<n>* buffer in the current perspective, in last-used order."
      (interactive)
      (let* ((all-buffers-in-emacs (buffer-list))
	     (all-buffers-in-persp (persp-buffer-list-restricted))
	     (sorted-buffers-in-persp (cl-remove-if-not (lambda (buf) (member buf all-buffers-in-persp)) all-buffers-in-emacs))
	     (first-vterm-buffer (cl-find-if (lambda (buf) (string-match-p "^\\*vterminal<[0-9]+>\\*$" (buffer-name buf))) sorted-buffers-in-persp)))
	(if first-vterm-buffer
	    first-vterm-buffer
	  nil)))

    (defun switch-to-last-persp-vterm ()
      "Switch to the last visited vterm buffer within the current perspective."
      (interactive)
      (let ((last-persp-vterm-buffer (ox/find-first-vterm-in-persp)))
	(message "vterm buffer is :%s" last-persp-vterm-buffer)
	(if last-persp-vterm-buffer
	    (switch-to-buffer last-persp-vterm-buffer)
	  (message "No last vterm buffer in this perspective to switch to.")
	  nil)))

    (global-set-key (kbd "C-c v") 'switch-to-last-persp-vterm)

    (defun switch-to-next-persp-vterm-from-last (&optional offset)
      "Switch to the next vterm buffer in the current perspective, starting from the last visited vterm buffer.
  OFFSET can be provided to skip a given number of buffers."
      (interactive "P")
      (let* ((offset (or offset 1))
	     (last-persp-vterm-buffer (ox/find-first-vterm-in-persp))
	     (all-vterm-buffers multi-vterm-buffer-list)
	     (persp-buffers (persp-buffer-list-restricted))
	     (persp-vterm-buffers (cl-intersection all-vterm-buffers persp-buffers :test 'eq))
	     (buffer-list-len (length persp-vterm-buffers))
	     (start-buffer (or last-persp-vterm-buffer (current-buffer)))
	     (my-index (cl-position start-buffer persp-vterm-buffers :test 'eq)))
	(if my-index
	    (let ((target-index (mod (+ my-index offset) buffer-list-len)))
	      (switch-to-buffer (nth target-index persp-vterm-buffers)))
	  (when persp-vterm-buffers
	    (switch-to-buffer (car persp-vterm-buffers))))))

    (defun switch-to-prev-persp-vterm-from-last (&optional offset)
      "Switch to the previous vterm buffer in the current perspective, starting from the last visited vterm buffer.
  OFFSET can be provided to skip a given number of buffers."
      (interactive "P")
      (switch-to-next-persp-vterm-from-last (- (or offset 1))))



    (global-set-key (kbd "C-}") 'switch-to-next-persp-vterm-from-last)
    (global-set-key (kbd "C-{") 'switch-to-prev-persp-vterm-from-last)



    ;; to share buffers in all perspectives
    ;;(defvar persp-shared-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
    ;;(add-hook 'persp-activated-functions
    ;;#'(lambda (_)
    ;;(persp-add-buffer persp-shared-buffers)))


    (setq persp-autokill-buffer-on-remove 'kill-weak)
    (add-hook 'window-setup-hook #'(lambda () (persp-mode 1)))

    (defvar my-dynamic-persps '()
      "List of dynamic perspectives, ordered by creation.")

    (defun my-update-dynamic-persps1 ()
      "Update `my-dynamic-persps` with the current list of perspectives."
      ;;(message persp-names-cache)
      ;;(message 'persp-names-current-frame-fast-ordered)
      ;;(setq my-dynamic-persps (persp-names-current-frame-fast-ordered))
      (setq my-dynamic-persps (copy-sequence persp-names-cache))
      ;;(message "Updated my-dynamic-persps: %s" (mapconcat 'identity my-dynamic-persps ", ")))
      )

    (defun my-update-dynamic-persps ()
      "Update `my-dynamic-persps` with the current list of perspectives from `persp-names-cache`."
      (setq my-dynamic-persps (remove "none" persp-names-cache)))

    (advice-add 'persp-kill :after (lambda (&rest _) (my-update-dynamic-persps)))
    (advice-add 'persp-switch :after (lambda (&rest _) (my-update-dynamic-persps)))
    (advice-add 'persp-add-new :after (lambda (&rest _) (my-update-dynamic-persps)))

    (defun my-switch-to-persp (name)
      "Switch to the perspective with NAME and update `my-dynamic-persps`."
      (interactive "sEnter perspective name: ")
      (when name
	(persp-switch name)))

    (defun my-switch-to-persp-by-number (number)
      "Switch to a perspective based on its position in `my-dynamic-persps`."
      (interactive "nPress the number key for the perspective: ")
      (if (eq number 0)
	  (my-switch-to-persp "none")
	(let ((name (nth (1- number) (remove "none" my-dynamic-persps))))
	  (if name
	      (my-switch-to-persp name)
	    (message "No perspective at position %d" number)))))

    ;; Initialize the list of dynamic perspectives at startup
    ;;(add-hook 'after-init-hook 'my-update-dynamic-persps)
    ;;(add-hook 'persp-mode-hook 'my-update-dynamic-persps)

    ;; Keybinding to create or switch to a named perspective
    (global-set-key (kbd "C-x p n") 'my-switch-to-persp)

    ;; Keybindings for Alt+numbers
    (dotimes (i 10)  ;; Loop from 0 to 9
      (let ((key (format "C-c %d" i)))
	(global-set-key (kbd key) `(lambda () (interactive) (my-switch-to-persp-by-number ,i))))))
  ;; (eval-after-load 'persp-mode
  ;;   '(my-update-dynamic-persps))
  (defvar my-persp-init-timer nil
    "Timer object for delayed initialization of my-dynamic-persps.")

  (defun my-check-persp-init ()
    "Check if perspectives other than 'none' are available in `persp-names-cache` and initialize if so."
    (when (and persp-names-cache (> (length persp-names-cache) 1))
      (my-update-dynamic-persps)
      (when my-persp-init-timer
	(cancel-timer my-persp-init-timer)
	(setq my-persp-init-timer nil))))

  (with-eval-after-load 'persp-mode
  (setq my-persp-init-timer (run-with-timer 0 1 'my-check-persp-init)))

(defun my-switch-to-project ()
  "Switch or open a project in its own perspective, with an option to add a new project."
  (interactive)
  (project-known-project-roots)
  (let* ((projects (append (mapcar #'identity (project-known-project-roots)) '("Add Project...")))
         (project (consult--read
                   projects
                   :prompt "Choose a project (or Add Project): "
                   :sort t)))
    (if (string-equal project "Add Project...")
        (setq project (read-directory-name "Select project directory: "))
          (message "Project added: %s" project))
      (my-switch-to-persp (file-name-nondirectory (directory-file-name project)))
      (project-switch-project project)))

;; Set font
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family my-linux-font :height my-font-size)
  (set-face-attribute 'default nil :family my-windows-font :height my-font-size))
;;(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140)

(set-frame-parameter nil 'alpha-background my-opacity) ; For current frame
(add-to-list 'default-frame-alist `(alpha-background . ,my-opacity)) ; For all new frames henceforth
(setq native-comp-async-report-warnings-errors nil) ;; Remove warning of compiled package with Emacs compiled with Native flag
(setq native-comp-deferred-compilation t) ;; To compile all site-lisp on demand (repos/AUR packages, ELPA, MELPA, whatever)
 (setq native-compile-prune-cache t) ;; And to keep the eln cache clean add 
;;(load-theme 'deeper-blue t)

;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(recentf-mode 1) ;; Enable the recent file mode to select with a number recent files
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
(save-place-mode 1) ;; set cursor at last location known when visiting a file
(savehist-mode 1)
(display-time-mode 1) ;;Display the time
(pixel-scroll-precision-mode 1)
(setq display-time-day-and-date 1)
(setq display-time-default-load-average nil) ;; Disable load time display

;; Nove customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Rever buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; Preserve pixel size when resizing (a must have in tiling WM to prevent useless gaps)
;; Until i find a solution to make awesome WM ignore ICCCM 
(setq frame-resize-pixelwise t)

;; Avoid constant errors on Windows about the coding system by setting the default to UTF-8.
(set-default-coding-systems 'utf-8)

;; Start automatically the daemon
(server-start)
;; Mode to log commands use clm/open-command-log-buffer to see them
(use-package command-log-mode
:straight t
:commands command-log-mode)
;; install all the icons
(use-package all-the-icons
:straight t)

;; make unique colors for each parentheses pair to see better delimitation
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Go to end of line and eval last sexp
(defun ox/eval()
  (interactive)
  (end-of-line)
  (eval-last-sexp nil))

(defun ox/compile (ox/command)
  (interactive "sCommand: ")
  ;;(setq-local buffer-save-without-query nil)
  (save-buffer)
  (compile (format "%s" ox/command))
;;(switch-to-buffer "*compilation*")
)

(defun ox/recompile()
(interactive)
(save-buffer)
(ignore-errors (kill-compilation)) ;; interrupt old compilation
(recompile)
;;(switch-to-buffer "*compilation*")
)


;; Better keybinding management 
(use-package general
  :straight t
  :after which-key
  :config
  (general-define-key
   "C-c C-v" 'compile-and-execute-c-code
   "C-c m" 'compile-or-recompile
   "C-c C-b" 'switch-to-previous-buffer
   "M-o" 'multi-vterm-dedicated-toggle
   "<escape>" 'keyboard-escape-quit)	; Make escape key quit prompts
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
    "b" '(frog-jump-buffer :which-key "frog-jump-buffer")

    ";" '(comment-or-uncomment-region :which-key "comment or uncomment region")
    "\\" '(ox/eval :which-key "eval-last-sexp")

    "ff" '(find-file :which-key "find-file")
    "fh" '((lambda () (interactive) (find-file "~/.emacs.d/Emacs.org")) :which-key "Open Habits.org")
    "fd" '(ox/ledeb-dired :which-key "dired-ledeb")
    "fp" '(consult-project-buffer :which-key "consult-project-buffer")
    "fe" '(consult-find :which-key "consult-find")
    "fg" '(consult-grep :which-key "consult-grep")
    "fr" '(consult-recent-file :which-key "Consult recent files")
    "fs" '(ox/sudo-find-file :which-key "Open files as sudo")
    "ft" '(treemacs-select-window :which-key "Open treemacs")
    "fc" '(consult-dir :which-key "consult-dir")

    "p" '(:ignore t :which-key "projects")
    "pp" '(my-switch-to-project :which-key "Open/switch project in persp")

    "c" '(:ignore t :which-key "compiling")
    "cc" '(compile :which-key "compile")
    "cd" '(ox/compile :which-key "ox/compile")
    "cr" '(ox/recompile :which-key "ox/recompile")))

(use-package which-key
   :straight t
   :after evil
  ;;:defer 0
  ;;:init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(defun ox/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
	(delete-minibuffer-contents))
    (backward-kill-word arg)))

(defun my-vertico-alt-done ()
  "Mimic the behavior of `ivy-alt-done' in Vertico."
  (interactive)
  (if-let ((file (vertico--candidate)))
      (if (file-directory-p file)
	  (vertico-insert)
	(vertico-exit))
    (vertico-exit-input)))


(use-package vertico
  :straight '(vertico :host github
		      :repo "minad/vertico"
		      :branch "main")
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ;("C-f" . vertico-exit)
	      ("C-f" . vertico-exit-input)
	      ;;("C-f" . my-vertico-alt-done)
	      ("TAB" . my-vertico-alt-done)
	      ("?" . minibuffer-completion-help)
	      ("RET" . minibuffer-force-complete-and-exit)
	      ;;("M-TAB" . minibuffer-complete)
	      ("M-TAB" . vertico-exit-input)
	      :map minibuffer-local-map
	      ("M-h" . ox/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (savehist-mode)
  (vertico-mode))

(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; (defvar +corfu-global-capes
;;   '(cape-yasnippet
;;     :completion
;;     cape-dict)
;;   "A list of global capes to be available at all times.
;; The key :completion is used to specify where completion candidates should be
;; placed, otherwise they come first.")

;; (defvar +corfu-capf-hosts
;;   '(lsp-completion-at-point
;;     eglot-completion-at-point
;;     elisp-completion-at-point
;;     tags-completion-at-point-function)
;;   "A prioritised list of host capfs to create a super cape onto from
;;   `+corfu-global-capes'.")

;; (defun +corfu--load-capes ()
;;   "Load all capes specified in `+corfu-global-capes'."
;;   (interactive)
;;   (when-let ((host (cl-intersection +corfu-capf-hosts completion-at-point-functions)))
;;     (setq-local
;;      completion-at-point-functions
;;      (cl-substitute
;;       (apply #'cape-capf-super (cl-substitute (car host) :completion (cl-pushnew :completion +corfu-global-capes)))
;;       (car host)
;;       completion-at-point-functions))))
;; (add-hook 'lsp-mode-hook #'+corfu--load-capes)
;; (add-hook 'change-major-mode-hook #'+corfu--load-capes)

(use-package corfu
  ;; :straight '(corfu :host github
  ;; 		    :repo "minad/corfu")
  :straight (corfu :files (:defaults "extensions/*")
		   :includes (corfu-info corfu-history))

  :bind (:map corfu-map
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous)
	      ("C-f" . corfu-insert)
	      ("C-e" . corfu-quit)
	      ("M-p" . corfu-popupinfo-scroll-up)
	      ("M-n" . corfu-popupinfo-scroll-down))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  ;;(corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  :config
  (general-define-key
   :states 'insert
   "C-e" 'corfu-quit)

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))


(use-package cape
  :straight t
  :after corfu
  :hook (lsp-after-initialize . ox/cape-test-hook) ;; Needed for cape capf to work 
  ;;:hook (lsp-after-open . ox/cape-test-hook) ;; Needed for cape capf to work 
  ;; :init
  ;; ;; NOTE: The order matters!
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-yasnippet)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; ;;(add-to-list 'completion-at-point-functions #'cape-line)
  ;; ;;(load-file "~/Documents/builds/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/cape-yasnippet.el")

  ;; ;; Silence the pcomplete capf, no errors or messages !
  ;; ;; Important for corfu
  ;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; ;; Ensure that pcomplete does not write to the buffer
  ;; ;; and behaves as a pure 'completion-at-point-function'
  ;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  ;; (add-hook 'eshell-mode-hook
  ;; 	    (lambda () (setq-local corfu-quit-at-boundary t
  ;; 				   corfu-quit-no-match t
  ;; 				   corfu-auto nil)
  ;; 	      (corfu-mode)))
  :init
  ;; (use-package company
  ;; :straight t)
  (defun ox/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
	  (cape-capf-buster #'lsp-completion-at-point))
    ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
    ;; listed when I want?
    ;;(add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'yasnippet-capf)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
  )
(defun ox/cape-test-hook ()
  (lsp-completion-mode -1)
  ;; (lambda () (lsp-completion-mode nil)
    (message "lsp-completion-mode running")
    (add-to-list 'completion-at-point-functions
		 (cape-capf-super  #'lsp-completion-at-point #'yasnippet-capf #'cape-file #'cape-dabbrev)))

  (use-package yasnippet-capf
    :straight '(yasnippet-capf :host github
			       :repo "elken/yasnippet-capf")
    :after cape yasnippet)



  (use-package orderless
    :straight t
    :init
    (setq completion-styles '(orderless)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles . (partial-completion))))))

  (defun ox/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

  (use-package consult
    :straight t
    :after which-key
    :demand t
    :bind (("C-s" . consult-line)
	   ("C-M-l" . consult-imenu)
	   ("C-M-j" . persp-switch-to-buffer*)
	   ([remap describe-key]      . helpful-key)
	   ([remap describe-command]  . helpful-command)
	   ([remap describe-variable] . helpful-variable)
	   ([remap describe-function] . helpful-callable)
	   :map minibuffer-local-map
	   ("C-r" . consult-history))
    :custom
    (consult-project-root-function #'ox/get-project-root)
    (completion-in-region-function #'consult-completion-in-region)
    :config
    ;; Customizing the find command to exclude git and node_modules folders
    (setq consult-find-args "find . -not ( -path */.git -path */node_modules -prune )")
    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
    ;; Add preview to consult-find
    (consult-customize consult-find :state (consult--file-preview))
    (ox/leader-keys
      "t" '(:ignore t :which-key "toggles")
      "tt" '(consult-theme :which-key "Load themes"))
    (consult-preview-at-point-mode))

  (use-package consult-lsp
    :straight t
    :after (lsp-mode consult))

;; A z like for consult
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)))
;; Force minibuffer to recognize the new bindings
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (define-key (current-local-map) (kbd "C-x C-j") 'consult-dir-jump-file)
            (define-key (current-local-map) (kbd "C-x C-d") 'consult-dir)))

;; An fzf like for consult (prefer to use consult-find/grep for the moment wait and see)
(use-package affe
  :straight t
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-.")
  ;; add preview to and affe-find
  (consult-customize affe-find :state (consult--file-preview)))

;; The default regular expression transformation of Consult is limited. It is recommended to configure Orderless as affe-regexp-compiler in Consult.
(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

  (use-package all-the-icons-completion
    :straight t
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :config
    ;;(all-the-icons-completion-mode)
    )

  (use-package marginalia
    :after vertico
    :straight t
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))



  (use-package embark
    :straight t
    :bind (("C-S-a" . embark-act)
	   :map minibuffer-local-map
	   ("C-d" . embark-act))
    :config

    ;; Show Embark actions via which-key
    (setq embark-action-indicator
	  (lambda (map)
	    (which-key--show-keymap "Embark" map nil nil 'no-paging)
	    #'which-key--hide-popup-ignore-command)
	  embark-become-indicator embark-action-indicator))

  (use-package embark-consult
    :straight '(embark-consult :host github
			       :repo "oantolin/embark"
			       :files ("embark-consult.el"))
    :after (embark consult)
    :demand t
    :hook
    (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package wgrep
  :straight t) ;; edit grep searches

(use-package harpoon
  :straight t
  :after (general which-key)
  :config
  (ox/leader-keys
    ;;"h" '(:ignore t :which-key "Org")
    "0" '(harpoon-add-file :whick-key "Add file to Harpoon")
    "1" '(harpoon-go-to-1 :which-key "harpoon file 1")
    "2" '(harpoon-go-to-2 :which-key "harpoon file 2")
    "3" '(harpoon-go-to-3 :which-key "harpoon file 3")
    "4" '(harpoon-go-to-4 :which-key "harpoon file 4")
    "5" '(harpoon-go-to-5 :which-key "harpoon file 5")
    "6" '(harpoon-go-to-6 :which-key "harpoon file 6")
    "7" '(harpoon-go-to-7 :which-key "harpoon file 7")
    "8" '(harpoon-go-to-8 :which-key "harpoon file 8")
    "9" '(harpoon-go-to-9 :which-key "harpoon file 9")))

(use-package hydra
  :straight t
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

(use-package rg
  :straight t
  :config
  ;;(rg-enable-default-bindings)
  (rg-enable-menu)
  )

(defun ox/sudo-find-file (file)
  "Open FILE as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo:root@localhost:" file))))

;; Better help view and features
(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-command helpful-key))

(use-package term
  :straight t
  :defer 0
  :config
  (setq explicit-shell-file-name "zsh"))
;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package vterm
  :straight t
  :defer 0
  :after (general which-key)
  :config
;; Remove mappings of alt+numbers from vterm
(dolist (key '("M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9" "M-0"))
    (define-key vterm-mode-map (kbd key) nil))
;; switch to last buffer in every mode with C-6
(evil-define-key '(visual insert normal) vterm-mode-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)
;; (evil-define-key '(visual insert normal) vterm-mode-map (kbd "C-{") 'multi-vterm-prev)
;; (evil-define-key '(visual insert normal) vterm-mode-map (kbd "C-}") 'multi-vterm-next)

  (setq vterm-max-scrollback 10000)
  (setq term-prompt-regexp "^[^❯\n]*[❯] *"))
;;(setq term-prompt-regexp "^[^❯\n]*[.*❯] .*"))
  ;;(setq term-prompt-regexp "^[^❯\n]*[❯] *"))
;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))
;; :hook (vterm-mode . (lambda ()
;; 			(evil-emacs-state))))
(use-package multi-vterm
  :straight t
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
    "sd" '(ox/ledeb-vterm :which-key "vterm ledeb")
    "se" '(eshell :whick-key "eshell"))
  (setq multi-vterm-dedicated-window-height-percent 40))
;; (add-hook 'vterm-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'buffer-face-mode-face) "Ubuntu Mono")
;;                  (buffer-face-mode t)))

(if (eq system-type 'gnu/linux)
	(setq explicit-shell-file-name "zsh")
    (setq explicit-shell-file-name "powershell.exe")
    (setq explicit-powershel.exe-args'()))

(use-package eshell-git-prompt
  :straight t
  :after eshell)

(defun ox/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)

  (setq eshell-history-size 10000
	eshell-buffer-maximun-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :straight t
  :hook (eshell-first-time-mode . ox/configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'multiline)

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

(defun my-switch-to-persp-vterm-by-number (number)
  "Target a vterm buffer in persp by NUMBER."
  (interactive "nPress the number key for the persp-vterm: ")
  (let* ((index 0)
	 (number (1- number))
	 (all-buffers-in-persp (reverse (persp-buffer-list-restricted)))
	 (persp-vterm-buffers (cl-remove-if-not (lambda (buf) (string-match-p "^\\*vterminal<[0-9]+>\\*$" (buffer-name buf))) all-buffers-in-persp)))
    (if persp-vterm-buffers
	(if (get-current-persp)
	    (progn
	      (while (< index number)
		(setq index (+ 1 index)))
	      (if (setq vterm-persp-p (elt persp-vterm-buffers index))
		  (switch-to-buffer vterm-persp-p)))
	  (switch-to-buffer (format "*vterminal<%d>*" (1+ number))))
      (message "No vterm buffer in the perspective")
      )
    ))

(let ((i 1))
(while (< i 10)  ;; Loop from 0 to 9
  (let* ((current-i i)
	 (key (format "C-c t %d" i))
	 (command-name (intern (format "my-persp-vterm-%d" i))))
     (defalias command-name
       (lambda()
		       (interactive)
		       (my-switch-to-persp-vterm-by-number current-i)))
     (keymap-global-set key command-name))
  (setq i (+ i 1))))

;; Dependencies for evil mode undo features
;; (use-package undo-tree
;;   :straight t
;; :init (global-undo-tree-mode)

;; :config
;; ;; Enable undo-tree mode

;; ;; Enable undo history saving
;; (setq undo-tree-auto-save-history t)

;; ;; Set the directory where undo histories will be saved
;; (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo-history"))))

(use-package undo-fu
  :straight t)
(use-package undo-fu-session
  :straight t
  :init (undo-fu-session-global-mode)
  )
;; For evil g; g, motions and last-change-register "."
(use-package goto-chg
  :straight t)

;;hook to start modes without evil mode
(defun ox/evil-hook ()
  (message "ox/evil-hook was called") ; add this line
  ;; Unbind RET key so emacs can use it instead of evil useful to make
  ;; org-return-follows-link working in evil-mode
  (define-key evil-motion-state-map (kbd "RET") nil) 
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
  ;;:straight t
  :straight '(evil :host github
		       :repo "emacs-evil/evil"
		       :branch "master")

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-fu)
  :hook (evil-mode . ox/evil-hook)
  :config
  (evil-set-undo-system 'undo-redo)
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
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-numbers
  :straight t
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

(use-package evil-mc
    :straight t
    :config
    (global-evil-mc-mode  1)

    (defun evil--mc-make-cursor-at-col (_startcol endcol orig-line)
      (move-to-column endcol)
      (unless (= (line-number-at-pos) orig-line)
        (evil-mc-make-cursor-here))
      )
    ;;; During visual selection point has +1 value
    (defun my-evil-mc-make-vertical-cursors (beg end)
      (interactive (list (region-beginning) (- (region-end) 1)))
      (evil-exit-visual-state)
      (evil-mc-pause-cursors)
      ;;; Because `evil-mc-resume-cursors` produces a cursor,
      ;;; we have to skip a current line here to avoid having +1 cursor
      (apply-on-rectangle #'evil--mc-make-cursor-at-col
                          beg end (line-number-at-pos))
      (evil-mc-resume-cursors)
      ;;; Because `evil-mc-resume-cursors` produces a cursor, we need to place it on on the
      ;;; same column as the others
      (move-to-column (evil-mc-column-number end))
      )

 (defun evil-mc-make-vertical-cursors (beg end)
      (interactive (list (region-beginning) (region-end)))
      (evil-mc-pause-cursors)
      (apply-on-rectangle #'evil--mc-make-cursor-at-col
                          beg end (line-number-at-pos (point)))
      (evil-mc-resume-cursors)
      (evil-normal-state)
      (move-to-column (evil-mc-column-number (if (> end beg)
                                                 beg
                                               end)))))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c k" . projectile-command-map)
  :init
  (when (file-directory-p my-project-path)
    (setq projectile-project-search-path `(,my-project-path)))
  (setq projectile-switch-projection-action #'projectile-dired))

(defun my/crunner ()
  "Make and Run a C program on a vterm buffer based on the makefile recipies
because compile mode is too slow"
  (interactive)
  (if (eq major-mode 'c-mode)
      (progn 
	(save-buffer)
	(let ((target (concat "make && time " "./" (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))) "\n"))
	      (switched nil))
	  (setq switched (switch-to-last-persp-vterm))
	  (unless (not (eq switched nil))
	    (multi-vterm))
	  (vterm-send-string target)))
    (print "Not in c-mode")))
  (ox/leader-keys
    "cv" '(my/crunner :which-key "Run C code in VTerm"))

;;(add-hook 'after-save-hook 'my/crunner)

(use-package eros
  :straight t
  :init
  (eros-mode 1))

(use-package nvm
  :straight t
  :defer t)

(use-package lua-mode
  :straight t
  :mode "\\.lua\\'")

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :config
  ;;(setq typescript-indent-level 2)
  )
(use-package prisma-mode
  :straight (:host github
  :repo "pimeys/emacs-prisma-mode"
  :branc "main")
)
(use-package emmet-mode
  :straight t
  :hook ((typescript-mode . emmet-mode))
  ;;(typescript-mode . emmet-preview-mode)))
  :config
  (ox/leader-keys
    "te" '(emmet-preview-mode :which-key "Emmet Preview Mode")))
;; (add-to-list 'emmet-jsx-major-modes tsx-ts-mode)
;; (add-to-list 'emmet-jsx-major-modes js2-jsx-mode))


;; Hide corfu suggestions and disable it when emmet-mode preview is working
(defun my-emmet-input-watcher (symbol newval operation where)
  (when (eq symbol 'emmet-preview-input)
    (if newval
        (progn
          (corfu-mode -1)
          (corfu-quit))
      (corfu-mode 1))))

(add-variable-watcher 'emmet-preview-input #'my-emmet-input-watcher)


;; Run code formatter on buffer contents without moving point, using RCS patches and dynamic programming. 
;; (use-package apheleia
;;   :straight t
;;   :config
;;   (apheleia-global-mode +1))

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :defer t)

(use-package flycheck-rust
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package web-mode
  :straight t
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  ;; (setq-default web-mode-code-indent-offset 2)
  ;; (setq-default web-mode-markup-indent-offset 2)
  ;; (setq-default web-mode-attribute-indent-offset 2)
  )

(use-package auto-rename-tag
  :straight t
  :hook ((typescript-mode . auto-rename-tag-mode)
         (js-mode . auto-rename-tag-mode)
         (mhtml-mode . auto-rename-tag-mode)
         (web-mode . auto-rename-tag-mode)))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :straight t)

;; Provides live interaction with JavaScript, CSS, and HTML in a web browser. Expressions are sent on-the-fly from an editing buffer to be evaluated in the browser, just like Emacs does with an inferior Lisp process in Lisp modes.
(use-package skewer-mode
  :straight t)

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config(require 'smartparens-config)
;; add a blank line when opening a {
  (sp-with-modes
      '(c++-mode objc-mode c-mode typescript-mode lua-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package flycheck
  :straight t
  :after lsp-mode
  :init (global-flycheck-mode))

(defun ox/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-tailwindcss
;;  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))
(use-package lsp-mode
  :straight t
  :hook
  ((lsp-mode . ox/lsp-mode-setup)
   (c-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (lua-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)
   (css-mode . lsp-deferred)
   (html-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (js-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  ;;(define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)
  :config
  (setq lsp-rust-server 'rust-analyzer) ; or 'rls

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (setq lsp-clients-angular-language-server-command					   ;;
  ;; '("node"										   ;;
  ;;   "/home/oxhart/.nvm/versions/node/v22.0.0/lib/node_modules/@angular/language-server" ;;
  ;;   "--ngProbeLocations"								   ;;
  ;;   "/home/oxhart/.nvm/versions/node/v22.0.0/lib/node_modules"			   ;;
  ;;   "--tsProbeLocations"								   ;;
  ;;   "/home/oxhart/.nvm/versions/node/v22.0.0/lib/node_modules"			   ;;
  ;;   "--stdio"))									   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq lsp-clients-angular-language-server-command
  '("node"
    "/usr/local/lib/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/usr/local/lib/node_modules"
    "--tsProbeLocations"
    "/usr/local/lib/node_modules"
    "--stdio"))

  ;; Configure Emmet LSP
   (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "emmet-ls" "--stdio")
                     :major-modes '(typescript-mode html-mode css-mode)
                     :server-id 'emmet-ls))
   (setq lsp-emmet-show-expanded-abbreviation t) ;; Show the expanded abbreviation in completion.
   (setq lsp-emmet-show-abbreviation-as-suggestion t) ;; Show abbreviation as suggestion.
  ;; Configure TailwindCSS Intellisense
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "tailwindcss-intellisense" "--stdio")
  ;;                   :major-modes '(typescript-mode html-mode css-mode)
  ;;                   :server-id 'tailwindcss))
  ;; Use lsp-mode everywhere possible
  (setq lsp-auto-guess-root t)

  (lsp-enable-which-key-integration t)
  ;; The path to lsp-mode needs to be added to load-path as well as the
  ;; path to the `clients' subdirectory.
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
  :commands (lsp lsp-deferred))

(ox/leader-keys
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  ;;"ls" 'counsel-imenu
  "ls" 'consult-lsp-diagnostics
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action
  "lg"  '(:ignore t :which-key "find")
  "lgr" 'lsp-find-references
  "lgg" 'lsp-find-definition
  "lge" 'lsp-treemacs-errors-list
  "lgq" 'lsp-treemacs-quickfix-list
  "lf" '(:ignore t :which-key "format")
  "l==" 'lsp-format-buffer
  "l=r" 'lsp-format-region
   )
(use-package lsp-ui
  :straight t
  :after lsp-mode
  ;;:commands lsp-ivy-workspace-symbol
  :hook (lsp-mode . lsp-ui-mode)
  ;;:custom(lsp-ui-doc-position 'bottom)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-ignore-duplicate t))

(use-package lsp-treemacs
  :straight t
  :after lsp-mode
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode t))
(use-package treemacs-evil
  :straight t
  :after lsp-treemacs)
(use-package treemacs-projectile
  :straight t
  :after lsp-treemacs)

;; (use-package dap-mode
;;   :straight t
;;   :custom
;;   (lsp-enable-dap-auto-configure nil)
;;   :config
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (require 'dap-node)
;;   (dap-node-setup))

(let* ((auth (auth-source-search :host "api.github.com" :user "S0mbr3^forge"))
       (token (funcall (plist-get (car auth) :secret))))
  ;; Now 'token' contains your GitHub token, and you can use it in your code.
  )

;; We are making magit getting the full buffer size
(use-package magit
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Allow to work with forges to get informations about repositories (notifications, issues, pull requests etc)
(use-package forge
:straight t
:after magit)

(defun my/vc-refresh-after-burying-magit (&rest args)
  "Refresh VC state after magit-status."
  (vc-refresh-state))

(defun my/vc-refresh-after-magit-checkout (&rest args)
  "Refresh VC state after magit-status."
  (vc-refresh-state))

 (advice-add 'magit-branch-and-checkout :after #'my/vc-refresh-after-magit-checkout)
 (advice-add 'magit-branch :after #'my/vc-refresh-after-magit-checkout)
 (advice-add 'magit-checkout :after #'my/vc-refresh-after-magit-checkout)
 (advice-add 'magit-refresh :after #'my/vc-refresh-after-magit-checkout)
(advice-add 'magit-mode-bury-buffer :after #'my/vc-refresh-after-burying-magit)


;;(add-hook 'magit-post-refresh-hook 'vc-refresh-state)

;; (defun refresh-vc-state (&rest r) (message "%S" (current-buffer))(vc-refresh-state))
;; (advice-add 'magit-checkout-revision :after 'refresh-vc-state '((name . "magit-refresh-on-checkout-revision")))
;; (advice-add 'magit-branch-create :after 'refresh-vc-state '((name . "magit-refresh-on-branch-create")))
;; (advice-add 'magit-branch-and-checkout :after 'refresh-vc-state '((name .  "magit-refresh-on-checkout-and-branch")))
;; (advice-add 'magit-branch-or-checkout :after 'refresh-vc-state '((name .  "magit-refresh-on-branch-or-checkout")))

;; (defun my/vc-refresh-state-after-shell-command (output)
;;   (when (string-match "Switched to branch" output)
;;     (vc-refresh-state)))

;; (add-hook 'comint-output-filter-functions 'my/vc-refresh-state-after-shell-command)

(defun ox/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(use-package org
  :straight t
  ;;:ensure nil
  ;;:pin org
  :commands (org-capture org-agenda)
  :hook ((org-mode . ox/org-mode-setup)
	 (org-mode . ox/org-mode-init)
	 (org-mode . (lambda()
		       (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
	 (org-mode . (lambda () (org-superstar-mode 0))))
  :config
  (message "hi from org-mode")
  (setq org-ellipsis " ↲"
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-startup-with-inline-images t
	org-agenda-time-grid
	'((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files my-org-files)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-with-latex-preview t) ;; Preview of latex symbols
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0)) ;; Change latex symbols size
  (setq org-return-follows-link t) ;; Allow to follow links using RET key
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))


	;;(setq python-indent-offset 4) ; Set indentation to 4 spaces (or any other desired value)


	(require 'org-indent)
	(require 'org-habit)
	(add-to-list 'org-modules 'org-habit)
	(setq org-todo-keywords
	      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
		(sequence "TODO(t)" "HABIT(h)" "|" "DONE(d!)")
		(sequence "BUYING(b1)" "|" "bought(B!)")
		(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")
		(sequence "A-PLAN()" "A-READY()" "A-ACTIVE()" "A-REVIEW()" "A-WAIT(@/!)" "A-HOLD()" "|" "A-COMPLETED(c)" "A-CANC(k@)")))

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
		("@learn" . ?L)
		("@wsl-configs" . ?w)
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

		("h" "Habit Tasks"
		 ((todo "HABIT"
			((org-agenda-overriding-header "Habit Tasks")))))

		("n" "Next Tasks"
		 ((todo "NEXT"
			((org-agenda-overriding-header "Next Tasks")))))

		("b" "Shopping Tasks"
		 ((todo "BUYING"
			((org-agenda-overriding-header "Shopping Tasks")))))

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
			 (org-agenda-files org-agenda-files)))))

		("v" "Activities Status"
		 ((todo "A-WAIT"
			((org-agenda-overriding-header "Waiting on External")
			 (org-agenda-files org-agenda-files)))
		  (todo "A-REVIEW"
			((org-agenda-overriding-header "In Review")
			 (org-agenda-files org-agenda-files)))
		  (todo "A-PLAN"
			((org-agenda-overriding-header "In Planning")
			 (org-agenda-todo-list-sublevels nil)
			 (org-agenda-files org-agenda-files)))
		  (todo "A-READY"
			((org-agenda-overriding-header "Ready to Go")
			 (org-agenda-files org-agenda-files)))
		  (todo "A-ACTIVE"
			((org-agenda-overriding-header "Active Activities")
			 (org-agenda-files org-agenda-files)))
		  (todo "A-COMPLETED"
			((org-agenda-overriding-header "Completed Activities")
			 (org-agenda-files org-agenda-files)))
		  (todo "A-CANC"
			((org-agenda-overriding-header "Cancelled Activities")
			 (org-agenda-files org-agenda-files)))))))

	(setq org-capture-templates
	      `(("t" "Tasks / Projects")
		("tt" "Task" entry (file+olp "~/syncthing/Sync/org-files/Tasks.org" "Inbox")
		 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

		("s" "Shopping / Projects")
		("ss" "Shop" entry (file+olp "~/syncthing/Sync/org-files/Shopping.org" "Inbox")
		 "* BUYING %?\n  %U\n  %a\n  %i" :empty-lines 1)

		("j" "Journal Entries")
		("jj" "Journal" entry
		 (file+olp+datetree "~/syncthing/Sync/org-files/Journal.org")
		 "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
		 ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
		 :clock-in :clock-resume
		 :empty-lines 1)
		("jm" "Meeting" entry
		 (file+olp+datetree "~/syncthing/Sync/org-files/Journal.org")
		 "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
		 :clock-in :clock-resume
		 :empty-lines 1)

		("w" "Workflows")
		("we" "Checking Email" entry (file+olp+datetree "~/syncthing/Sync/org-files/Journal.org")
		 "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

		("v" "Activities")
		("va" "Activities idea" entry (file+olp+datetree "~/syncthing/Sync/org-files/Journal.org")
		 "* A-PLAN %? :activities:" :clock-in :clock-resume :empty-lines 1)

		("m" "Metrics Capture")
		("mw" "Weight" table-line (file+headline "~/syncthing/Sync/org-files/Metrics.org" "Weight")
		 "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

	(define-key global-map (kbd "C-c j")
		    (lambda () (interactive) (org-capture nil "jj")))
	(ox/leader-keys
	  "o" '(:ignore t :which-key "org")
	  "oa" '(org-agenda :which-key "open org-agenda")
	  "ot" '(org-todo-list :which-key "open all todo lists")
	  "oc" '(org-capture :which-key "open org-capture")
	  "oh" '((lambda () (interactive) (find-file "~/syncthing/Sync/org-files/Habits.org")) :which-key "Open Habits.org")
	  "ow" '((lambda () (interactive) (find-file "~/syncthing/Sync/org-files/Metrics.org")) :which-key "Open Metrics.org")))


  ;; Center the text, and set a max column width to go next line in org mode
  (defun ox/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
	  visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :straight t
    :hook (org-mode . ox/org-mode-visual-fill))

  (use-package org-superstar
    :straight t
    :after org
    :config
    ;;(setq org-superstar-hide-leading-stars t)
    (setq org-superstar-leading-bullet " ")
    ;; Hide away leading stars on terminal.
    (setq org-superstar-leading-fallback ?\s))

(use-package org-modern
  :straight t
  ;;:disabled t
:config
(with-eval-after-load 'org (global-org-modern-mode)))

(use-package ob-typescript
  :straight t)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (makefile . t)
     (shell . t)
     (typescript . t)
     (gnuplot .t )
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
;; (defun ox/org-babel-tangle-config ()
;;   (when (string-equal (buffer-file-name)
;; 		      (expand-file-name "~/terminalConfigs/.dotfiles/emacs/.emacs.d/orgFiles/Emacs.org"))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;;     (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ox/org-babel-tangle-config)))




(defun ox/org-buffer-property (name)
  "Get the value of a buffer-wide Org property NAME."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^#\\+PROPERTY:.*" name " +\\(.*\\)") nil t)
        (progn
          (message "Found property %s with value %s" name (match-string 1))
          (match-string 1))
      (message "Property %s not found" name)
      nil)))

(defun ox/org-babel-tangle-config ()
  "Automatically tangle Org files with the :auto-tangle property set to t."
  (message "Running ox/org-babel-tangle-config")
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(defun ox/check-and-add-tangle-hook ()
  "Check for the auto-tangle property and add tangle hook if needed."
  (message "Checking for auto-tangle property...")
  (let ((auto-tangle (ox/org-buffer-property ":auto-tangle")))
    (if (and auto-tangle (string= auto-tangle "t"))
        (progn
          (message "Auto-tangle property found. Adding after-save-hook...")
          (add-hook 'after-save-hook #'ox/org-babel-tangle-config nil 'local))
      (message "Auto-tangle property not set to t"))))

(defun ox/org-mode-init ()
  "Initialize Org mode with tangle hook check."
  (message "Initializing Org mode...")
  (ox/check-and-add-tangle-hook))

(use-package org-fc
  :straight t
  :after org
  ;; :hook ((org-fc-review-flip-mode org-fc-review-rate-mode) . my-check-org-fc-and-set-evil-state )
  :config
  (setq org-fc-directories '("~/syncthing/Sync/org-files"))
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c f") 'org-fc-hydra/body))
  (require 'org-fc-hydra))

(defun my-check-org-fc-and-set-evil-state ()
  "Switch to evil-emacs-state if flycheck-mode is active."
  (if (or org-fc-review-flip-mode org-fc-review-rate-mode)
      (evil-emacs-state)
    (evil-normal-state)))

(add-hook 'org-fc-review-flip-mode-hook 'my-check-org-fc-and-set-evil-state)
(add-hook 'org-fc-review-rate-mode-hook 'my-check-org-fc-and-set-evil-state)

(use-package org-transclusion
  :straight t
  :after org
  :config
  (setq org-transclusion-add-all-on-activate t
  org-transclusion-exclude-elements '(property-drawer keyword))
  )
(add-hook 'org-mode-hook  #'(lambda() (org-transclusion-mode)))

(use-package org-roam
  :straight t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
   ;;(org-roam-db-autosync-mode)
  (setq org-roam-directory (file-truename "~/syncthing/Sync/org-roam"))
   (org-roam-db-autosync-enable)
)

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
:straight nil
:disabled t
:after tree-sitter
;;:defer 0
)
(use-package tree-sitter
:straight t
;;:after tree-sitter-langs
:config
;; Loading tree-sitter-modules from casouri/tree-sitter-module
;; Preventing from manually installing tree-sitter grammars
(setq treesit-extra-load-path '("/home/oxhart/builds/tree-sitter-module/dist"))
;; Activate tree-sitter globally (minor mode registered on every buffer
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

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode c-mode makefile-gmake-mode) . indent-bars-mode))

(use-package aggressive-indent
  :straight nil
  :disabled t
  :hook((python-base-mode yaml-mode c-mode makefile-gmake-mode) . agressive-indent-mode)
  :config
  (global-aggressive-indent-mode 1))

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
    :straight t
    :hook (dired-mode . all-the-icons-dired-mode))


  (use-package ranger
    ;;:straight t
    :straight '(ranger :host github
		       ;;:local-repo "/home/oxhart/builds/ranger.el/"
		       :repo "S0mbr3/ranger.el"
		       :branch "ranger-setup-image-preview")
    :config
    (global-set-key (kbd "C-c d") 'ranger)
    (setq ranger-show-literal nil) ;; if nil show documents intead of text representation

    ;; Make the header line cleaned when quiting ranger or it stays (sound like a bug)
    (defun my/ranger-clear-header-line ()
      "Clear the header line."
      (setq header-line-format nil))

    (advice-add 'ranger-close :after #'my/ranger-clear-header-line))

    (use-package dired-hide-dotfiles
      :unless (featurep 'ranger)
      :straight t
      :hook (dired-mode . dired-hide-dotfiles-mode)
      :config
      (evil-collection-define-key 'normal 'dired-mode-map
	"H" 'dired-hide-dotfiles-mode))

    (use-package dired-preview
      :unless (featurep 'ranger)
      :straight t
      :hook (dired-mode . dired-preview-mode)
      :config
      (dired-preview-global-mode 1))

    (use-package dired-open
      :unless (featurep 'ranger)
      :straight t
      :after dired
      ;;:commands (dired dired-jump)
      :config
      ;; Strange behaviors not picking always the good program automatically
      ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
      (setq dired-open-extensions '(("png" . "feh")
				    ("mkv" . "mpv"))))

(require 'tramp)
(use-package ssh-config-mode
  :straight t
  :mode (("~/.ssh/config\\'" . ssh-config-mode)
         ("sshd?_config\\'" . ssh-config-mode)))
(setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]\n#-%>❯]*#?[]#-%>❯][[:blank:]]*")
(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/sshx:ledeb:")
                   "remote-shell" "/usr/bin/zsh"))
(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:ledeb:")
                   "remote-shell" "/usr/bin/zsh"))
(setq vterm-tramp-shells '(("docker" "/bin/sh")
			   ("ssh" "/usr/bin/zsh")))
(defun ox/ledeb-vterm ()
  "Open vterm in ledeb server"
  (interactive)
  (print "salut")
  (let ((default-directory "/ssh:ledeb:"))
    (multi-vterm)))

(defun ox/ledeb-dired ()
  "Open dired in ledeb server"
  (interactive)
    (dired "/ssh:ledeb:"))

(use-package gnuplot
  :straight t)
(use-package gnuplot-mode
  :straight t
  :config
  ;; automatically open files ending with .gp or .gnuplot in gnuplot mode
(setq auto-mode-alist 
      (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist)))

;; When using compile or recompile command if there is some colord characters
;; it does not format well I had to use ansi-color with a hook in compilation mode

;; (require 'ansi-color)

;; (defun my-ansi-colorize-buffer ()
;;   (let ((buffer-read-only nil))
;;     (ansi-color-apply-on-region (point-min) (point-max))))

;; (add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer)

;; (ignore-errors
;;   (require 'ansi-color)
;;   (defun my-colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
;;   (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; Builtin since emacs 28
(use-package ansi-color
:ensure nil
:hook (compilation-filter . ansi-color-compilation-filter)
:config
;;(setq ansi-color-for-comint-mode t)
(setq compilation-environment '("TERM=xterm-256color")))
;;(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;; (defun colorize-compilation-buffer ()
;;   (when (eq major-mode 'compilation-mode)
;;     (ansi-color-apply-on-region compilation-filter-start (point-max))))

;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (use-package xterm-color
;; :straight t
;; :config
;; (setq compilation-environment '("TERM=xterm-256color"))

;; (defun my/advice-compilation-filter (f proc string)
;;   (funcall f proc (xterm-color-filter string)))

;; (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

(use-package auto-package-update
  :straight t
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

(use-package chess
:straight t)

;;all-the-icons-ivy package is required to activate icons support in frog-jump-buffer
(use-package all-the-icons-ivy
  :straight t)
(use-package frog-jump-buffer
  :straight t
  :hook (frog-menu-after-init .  ox/custom-frog-face-outrun)
  :config
  (setq frog-jump-buffer-use-all-the-icons-ivy t)
  )
(defun ox/custom-frog-face-outrun ()
  "Change, faces for frog-meny and posframe to adapt frog-menu-buffer with the doom-outrun-electric theme"
  ;; Custom faces for frog-menu and posframe
  (custom-set-faces
   ;; Frog Menu Faces
   `(frog-menu-border ((t (:background unspecified)))) ;; No border color
   `(frog-menu-posframe ((t (:background unspecified :foreground "inherit")))) ;; Transparent background
   `(frog-menu-prompt-face ((t (:foreground unspecified :weight bold :background unspecified)))) ;; Transparent prompt background
   `(frog-menu-actions-face ((t (:foreground unspecified :background unspecified)))) ;; Transparent actions background
   `(frog-menu-candidates-face ((t (:background unspecified :foreground unspecified)))) ;; Transparent candidates background
   `(frog-menu-action-keybinding-face ((t (:foreground unspecified :background unspecified)))) ;; Transparent action keybinding background
   `(frog-menu-posframe-background-face ((t (:background unspecified)))) ;; Transparent posframe background

   ;; Posframe Faces
   `(posframe-background-face ((t (:background unspecified)))) ;; Transparent posframe background
   )

  ;; (custom-set-faces
  ;;  ;; Frog Menu Faces
  ;;  `(frog-menu-border ((t (:background "#ffd400" :foreground "#ffd400" :weight bold)))) ;; Brighter border color
  ;;  `(frog-menu-prompt-face ((t (:foreground "#ff2afc" :weight bold)))) ;; Prompt color
  ;;  `(frog-menu-actions-face ((t (:foreground "#a7da1e")))) ;; Actions color
  ;;  `(frog-menu-candidates-face ((t (:background "#0c0a20" :foreground "#f2f3f7")))) ;; Candidates color
  ;;  `(frog-menu-action-keybinding-face ((t (:foreground "#42c6ff")))) ;; Action keybinding color
  ;;  `(frog-menu-posframe-background-face ((t (:background "#0c0a20")))) ;; Posframe background color

  ;;  ;; Posframe Faces
  ;;  `(posframe-background-face ((t (:background "#0c0a20")))) ;; Posframe background color
  ;;  )

  )
;; (defun my/frog-menu-hook () (
;; 			       (setq-local avy-background nil))
;; 	 (add-hook 'frog-menu-after-init-hook 'my/frog-menu-hook))

;; Make gc pauses faster by decreasubg tge threshold.
(setq gc-cons-threshold (* 2 1000 000))
