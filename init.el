;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Define and initialise package repositories (package.el is turned off in early-init.el)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install Use-Package and configure to use Straight
(straight-use-package 'use-package)
(use-package straight
	     :custom (straight-use-package-by-default t))

;; UI stuff:
;; Startup
(setq inhibit-startup-message t
      cursor-type 'bar)
;; Basic UI changes:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 5)
(when window-system (set-frame-size (selected-frame) 120 80))

(defun display-line-numbers-hook ()
  (display-line-numbers-mode t)
  )

(add-hook 'prog-mode-hook 'display-line-numbers-hook)
(setq display-line-numbers-type 'relative)

;; Set up the visible bell
(setq visible-bell t)

;; Faces
(defvar jeff/default-font-size 120)

(set-face-attribute 'default nil :font "Fira Code" :height jeff/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 180 :weight 'regular)
;; Add a theme for eye-ease
;; (use-package nord-theme
;;   :ensure t
;;   :config (load-theme 'nord t))
(use-package doom-themes
  :init (load-theme 'doom-nord t))

;; Note: the first time you load this config you'll need to run the following interactively:
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package general
  :config
  (general-create-definer jeff/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (jeff/leader-keys
   "v" '(:ignore v :which-key "visuals")
   "vt" '(counsel-load-theme :which-key "choose theme")
   "b" '(:ignore t :which-key "buffers")
   "bs" '(counsel-switch-buffer :which-key "switch buffer")
   "bk" '(kill-buffer :which-key "kill buffer")))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(jeff/leader-keys
  "vs" '(hydra-text-scale/body :which-key "scale text"))

;; Ivy completion/Ivy adjacent items
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
  ("C-x b" . counsel-ibuffer)
  ("C-x C-f" . counsel-find-file)
  :map minibuffer-local-map
  ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :demand
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
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	iv-count-format "%d/%d "))


(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; 'helpful' for describing functions:

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
(global-set-key (kbd "C-h C") #'helpful-command)

;; Magit 
(use-package magit
  :ensure t) 

;; (use-package evil-magit
;;   :after magit)

;; add some global leader-key bindings for magit
(jeff/leader-keys
  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gd" 'magit-diff-unstaged
  "gc" 'magit-branch-or-checkout
  "gl" '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb" 'magit-branch
  "gP" 'magit-push-current
  "gp" 'magit-pull-branch
  "gf" 'magit-fetch
  "gF" 'magit-fetch-all
  "gr" 'magit-rebase
  "gi" 'magit-init
  "gh" 'magit-info)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package rg
  :ensure t)

(use-package counsel-projectile
  :ensure t 
  :config (counsel-projectile-mode))

(jeff/leader-keys
  "p" 'projectile-command-map)

;; Org-mode

(defun jeff/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun jeff/org-mode-font-setup ()
  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  )


(use-package org
  :hook (org-mode . jeff/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (jeff/org-mode-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)))
(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("cl" . "src clojure"))


(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


