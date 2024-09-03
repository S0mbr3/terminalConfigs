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
    (ox/leader-keys
      "t" '(:ignore t :which-key "toggles")
      "tt" '(consult-theme :which-key "Load themes"))
    (consult-preview-at-point-mode))

  (use-package consult-lsp
    :straight t
    :after (lsp-mode consult))

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
