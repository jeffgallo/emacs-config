;; -*- lexical-binding: t; -*- 
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
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jeff Gallo"
      user-mail-address "jeffreyfgallo@gmail.com")

;; Startup
 (setq inhibit-startup-message t
       cursor-type 'bar)
 ;; Confirm on quit
 (setq confirm-kill-processes nil)

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
 (defvar jeff/default-font-size 110)
 (set-face-attribute 'default nil :font "Fira Code" :height jeff/default-font-size)
 (set-face-attribute 'italic nil :family "Hack")
 ;; Set the fixed pitch face
 (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
 ;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)
 ;; Add a theme for eye-ease
  (use-package modus-themes
    :ensure
    :init
    (setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-syntax '(faint)
      modus-themes-mode-line '(accented borderless)
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-region '(bg-only no-extend)
 ;; 	modus-themes-org-blocks 'tinted-background
 ;; 	modus-themes-scale-headings t
 ;; 	modus-themes-headings
 ;; 	'((1 . (rainbow overline background 1.4))
 ;; 	(2 . (rainbow background 1.3))
 ;; 	(3 . (rainbow bold 1.2))
 ;; 	(t . (semilight 1.1)))
      )

    (modus-themes-load-themes)
    :config
    (modus-themes-load-vivendi))
 ;; (use-package nord-theme
 ;;   :ensure t
 ;;   :config (load-theme 'nord t))
 ;; (use-package doom-themes
 ;;   :init (load-theme 'doom-nord t))
 ;; Note: the first time you load this config you'll need to run the following interactively:
 ;; M-x all-the-icons-install-fonts
 (use-package all-the-icons)
 (use-package doom-modeline
   :ensure t
   :init (doom-modeline-mode 1))
 (use-package rainbow-delimiters
   :hook (prog-mode . rainbow-delimiters-mode))
 (use-package paren
   :straight (:type built-in)
   :ensure nil
   :config
   (show-paren-mode +1))

 (use-package which-key
   :init (which-key-mode)
   :diminish which-key-mode
   :config
   (setq which-key-idle-delay 1))

 ;; browse-kill-ring
 (use-package browse-kill-ring)
 ;; Beacon mode
 (use-package beacon
   :init (beacon-mode 1))

(use-package exec-path-from-shell
   :config (exec-path-from-shell-initialize))

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
    (evil-set-initial-state 'elfeed-mode 'emacs)
(evil-set-initial-state 'pocket-reader-mode 'emacs)

    (evil-mode 1)
    )

  (use-package evil-collection
    :after evil
    :ensure t
    :init
    (setq evil-collection-magit-use-z-for-folds nil)
    :config (evil-collection-init))

  (use-package general
    :config
    (general-evil-setup)

    (general-create-definer jeff/leader-keys
      :keymaps '(normal insert visual emacs org-agenda-mode-map)
      ;;:states '(normal visual)
      :prefix "SPC"
      :global-prefix "C-SPC"
      :prefix-command 'tyrant-prefix-command
      :prefix-map 'tyrant-prefix-map)
    (jeff/leader-keys
     "v" '(:ignore t :which-key "visuals")
     "vt" '(counsel-load-theme :which-key "choose theme")
     "f" '(:ignore t :which-key "find-files")
     "ff" '(find-file :which-key "find-file")
     "fp" '(find-file-projectile :which "find-file-projectile")
     "b" '(:ignore t :which-key "buffers")
     "bs" '(counsel-switch-buffer :which-key "switch buffer")
     "bi" '(ibuffer :which-key "buffers")
     "bk" '(kill-current-buffer :which-key "kill current buffer")
     "bK" '(kill-buffer :which-key "kill buffer from list")
     "r" '(:ignore t :which-key "read")
     "re" '(elfeed :which-key "elfeed")
     "rp" '(pocket-reader :which-key "pocket")
     "e" '(mu4e :which-key "Email")
     "o" '(:ignore t :which-key "org")
     "oa" '(org-agenda :which-key "org-agenda")
     "oc" '(org-capture :which-key "org-capture")
     "d" '(dired :which-key "dired"))) 

  (use-package hydra)
  (defhydra hydra-text-scale (:timeout 5)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))
  (jeff/leader-keys
    "vs" '(hydra-text-scale/body :which-key "scale text"))

(use-package smex)
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
          ivy-count-format "%d/%d "))
(setq ivy-re-builders-a-list
      '((swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)
  (use-package ivy-rich
    :init
    (ivy-rich-mode 1))

(use-package dired
     :straight (:type built-in)
     :ensure nil
     :commands (dired dired-jump)
     :bind (("C-x C-j" . dired-jump))
     ;;:custom ((dired-listing-switches "-agho --group-directoryies-first"))
     :config
   (evil-collection-define-key 'normal 'dired-mode-map
     "h" 'dired-single-up-directory
     "l" 'dired-single-buffer))

 (use-package dired-single)

 (use-package all-the-icons-dired
   :hook (dired-mode . all-the-icons-dired-mode)
   :init (setq all-the-icons-dired-monochrome nil))

 (use-package dired-open
   :config
   ;; Doesn't work as expected!
   ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
   (setq dired-open-extensions '(("mkv" . "mpv"))))

 (use-package dired-hide-dotfiles
   :hook (dired-mode . dired-hide-dotfiles-mode)
   :config
   (evil-collection-define-key 'normal 'dired-mode-map
     "H" 'dired-hide-dotfiles-mode)
)

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

(defun jeff/org-mode-setup ()
   ;; (org-indent-mode)
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
;;    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    )

  (use-package org
    :hook (org-mode . jeff/org-mode-setup)
    :config
    (setq org-ellipsis " ▾"
	  org-hide-emphasis-markers t)
    ;;(jeff/org-mode-font-setup)
    )
(setq org-clock-sound "~/Downloads/elevator-announcement-bells.wav")

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

(use-package org-super-agenda
    :after org-agenda
    :config
    (setq org-super-agenda-groups '(
                                    ;;(:auto-group t)
                                    (:name "Today"
                                     :time-grid t
                                     )
                                    (:name "Projects"
                                     :todo "PROJECT")
                                    (:name "Tasks to Refile"
                                     :and (:todo ("TODO" "NEXT" "PROJECT")
                                           ;;:tag "REFILE"
                                           )
                                     )
                                    (:name "Notes to Refile"
                                     :and (
                                           ;;:tag ("REFILE" "NOTE" "MEETING")
                                           :not(:todo ("TODO" "NEXT" "PROJECT"))
                                     ))
                                    ))
    (org-super-agenda-mode))

  ;; ORG Mode
  (setq jeff/org-agenda-files
     (list  "~/Nextcloud/org/TessNet.org"
            "~/Nextcloud/org/Review.org"
            "~/Nextcloud/org/TODO.org"
            "~/Nextcloud/org/Habits.org"
            "~/Nextcloud/org/Chores.org"
            "~/Nextcloud/org/Journal.org"
            "~/Nextcloud/org/REFILE.org"))
(setq org-agenda-files jeff/org-agenda-files)
  
  (setq org-refile-targets '((nil :maxlevel . 2)
                                (org-agenda-files :maxlevel . 2)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (setq safe-local-variable-values
    '((org-download-image-dir . "~/Nextcloud/org/Journal-Images")))

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
   (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/Nextcloud/org/REFILE.org")
                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("r" "respond" entry (file "~Nextcloud/org/REFILE.org")
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/Nextcloud/org/REFILE.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+olp+datetree "~/Nextcloud/org/Journal.org")
                 "* %?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/Nextcloud/org/REFILE.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/Nextcloud/org/REFILE.org")
                 "* Meeting with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Project" entry (file "~/Nextcloud/org/REFILE.org")
                 (file "~/Nextcloud/org/templates/ProjectTemplate.org") :clock-in t :clock-resume t)
                ("W" "Weekly Review" entry (file+olp+datetree "~/Nextcloud/org/Journal.org")
                 (file "~/Nextcloud/org/templates/WeeklyReviewTemplate.org") :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/Nextcloud/org/REFILE.org")
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")

                )))

  ;; KEYWORDS
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "PROJECT(p)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  (setq org-src-tab-acts-natively t)

       ; position the habit graph on the agenda to the right of the default
       (setq org-habit-graph-column 50)
           ;; Toggle line mode for org-agenda
           (add-hook 'org-agenda-mode-hook
                     (lambda ()
                       (visual-line-mode -1)
                       (toggle-truncate-lines 1)))

           ;; Set agenda view columns
           (setq org-agenda-tags-column 80)

             ;; Standard key bindings
             (global-set-key "\C-cl" 'org-store-link)
             (global-set-key "\C-ca" 'org-agenda)
             (global-set-key "\C-cb" 'org-iswitchb)
             (global-set-key "\C-cc" 'org-capture)
             (global-set-key (kbd "C-c o")
                             (lambda () (interactive) (find-file "~/Nextcloud/org/TODO.org")))
             (setq org-log-done t)
             (setq org-directory "~/Nextcloud/org")
             (setq org-default-notes-file "~/Nextcloud/org/REFILE.org")

(use-package org-roam
    :ensure t
    :demand t
    :custom
    (org-roam-directory (file-truename "~/Nextcloud/org/roam/"))
    (org-roam-completion-everywhere t)
    (org-roam-capture-templates
'(("d" "default" plain
   "%?"
   :if-new (file+head "%<%Y%m%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
  ("l" "programming language" plain
   (file "~/Nextcloud/org/templates/programming-language.org")
   :if-new (file+head "%<%Y%m%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
  ("b" "book notes" plain
   (file "~/Nextcloud/org/templates/book-notes.org")
   :if-new (file+head "%<%Y%m%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
  ("p" "tessnet project" plain
   (file "~/Nextcloud/org/templates/TessNetProjectTemplate.org")
   :if-new (file+head "%<%Y%m%d-%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: :Project: :TESSNET: :work:\n")
   :unnarrowed t)
   ("P" "project" plain
         (file "~/Nextcloud/org/templates/ProjectTemplate.org")
         :if-new (file+head "%<%Y%m%d-%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n")
         :unnarrowed t)
  ))

    :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-insert-immediate)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-y" . completion-at-point))
      :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
(org-roam-db-autosync-mode)
;; If using org-roam-protocol
(require 'org-roam-protocol))
(jeff/leader-keys
  "n" '(:ignore t :which-key "org-roam-map")
  "nc" '(org-roam-capture :which-key "org-roam-capture")
  "nf" '(org-roam-node-find :which-key "org-roam-node-find")
  "ng" '(org-roam-graph :which-key "org-roam-graph")
  "ni" '(org-roam-node-insert :which-key "org-roam-node-insert")
  "nI" '(org-roam-insert-immediate :which-key "org-roam-insert-immediate")
  "nj" '(org-roam-dailies-capture-today :which-key "org-roam-dailies-capture-today")
  "nl" '(org-roam-buffer-toggle :which-key "org-roam-buffer-toggle")
  "nr" '(jeff/org-roam-refresh-agenda-list :which-key "org-roam-refresh-agenda-list")
  "nb" '(jeff/org-roam-capture-inbox :which-key "org-roam-capture-inbox")
  "nt" '(jeff/org-roam-capture-task :which-key "org-roam-capture-task"))

(defun org-roam-insert-immediate (arg &rest args)
(interactive "P")
(let ((args (cons arg args))
      (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                '(:immediate-finish t)))))
  (apply #'org-roam-node-insert args)))

(defun jeff/org-roam-filter-by-tag (tag-name)
   (lambda (node)
    (member tag-name (org-roam-node-tags node))))

 (defun jeff/org-roam-list-notes-by-tag (tag-name)
   (mapcar #'org-roam-node-file
           (seq-filter
            (jeff/org-roam-filter-by-tag tag-name)
            (org-roam-node-list))))

 (defun jeff/org-roam-refresh-agenda-list ()
   (interactive)
   (setq org-agenda-files (append jeff/org-agenda-files (jeff/org-roam-list-notes-by-tag "Project")))
   (setq org-refile-targets '((nil :maxlevel . 2)
                            (org-agenda-files :maxlevel . 2))))

(jeff/org-roam-refresh-agenda-list)

(defun jeff/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'jeff/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun jeff/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'jeff/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (jeff/org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain
       (file "~/Nextcloud/org/templates/ProjectTemplate.org")
       :if-new (file+head "%<%Y%m%d-%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n")
       :unnarrowed t))))

(global-set-key (kbd "C-c n p") #'jeff/org-roam-find-project)

(defun jeff/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)

(defun jeff/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'jeff/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (jeff/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(global-set-key (kbd "C-c n t") #'jeff/org-roam-capture-task)

(defun jeff/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (jeff/org-roam-copy-todo-to-today))))

(use-package elfeed
  :bind
   (:map elfeed-search-mode-map
                 ("A" . elfeed-show-all)
                 ("T" . elfeed-show-tech)
                 ("N" . elfeed-show-news)
                 ("E" . elfeed-show-emacs)
                 ("D" . elfeed-show-daily)
                 ("q" . elfeed-save-db-and-bury)))
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Nextcloud/elfeed.org")))
  (add-hook 'elfeed-search-mode-hook 'turn-off-evil-mode)
  (add-hook 'elfeed-show-mode-hook 'turn-off-evil-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; elfeed feed reader                                                     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;shortcut functions
  (defun elfeed-show-all ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))
(defun elfeed-show-tech ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-tech"))
(defun elfeed-show-news ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-news"))
  (defun elfeed-show-emacs ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))
  (defun elfeed-show-daily ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))

  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

;; set EWW as default browser
 ;;(setq browse-url-browser-function 'eww-browse-url)

;; browse article in gui browser instead of eww
(defun jeff/elfeed-show-visit-gui ()
  "Wrapper for elfeed-show-visit to use gui browser instead of eww"
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/open"))
    (elfeed-show-visit t)))

(define-key elfeed-show-mode-map (kbd "B") 'jeff/elfeed-show-visit-gui)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(defun jeff/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
:hook (lsp-mode . lsp-ui-mode)
:custom
(lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
 :after lsp)

(use-package lsp-ivy)

(use-package dap-mode)
(require 'dap-firefox)
(require 'dap-chrome)
(require 'dap-node)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-show-numbers t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (setq company-box-icons-alist 'company-box-all-the-icons)
  (company-box))

(global-company-mode)
 (let ((map company-active-map))
  (define-key map (kbd "<tab>") 'company-complete-selection)
  (define-key map (kbd "<return>") nil)
  (define-key map (kbd "RET") nil))

(use-package clojure-mode)
(use-package cider)
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(use-package json-mode)
(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred))
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(setq plantuml-jar-path "~/.java/plantuml-1.2021.16.jar")
(setq org-plantuml-jar-path "~/.java/plantuml-1.2021.16.jar")
(setq plantuml-default-exec-mode 'jar)

(use-package csharp-mode)
( add-hook 'csharp-mode-hook 'lsp  )

(use-package rust-mode)
(add-hook 'rust-mode-hook
	  (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
        (lambda () (prettify-symbols-mode)))

(use-package magit
  :ensure t)
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

(use-package copilot
    :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
    :ensure t
    )

  ;; borrowed from: https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;;   (defun rk/no-copilot-mode ()
;;   "Helper for `rk/no-copilot-modes'."
;;   (copilot-mode -1))

;; (defvar rk/no-copilot-modes '(shell-mode
;;                               inferior-python-mode
;;                               eshell-mode
;;                               term-mode
;;                               vterm-mode
;;                               comint-mode
;;                               compilation-mode
;;                               debugger-mode
;;                               dired-mode-hook
;;                               compilation-mode-hook
;;                               flutter-mode-hook
;;                               minibuffer-mode-hook)
;;   "Modes in which copilot is inconvenient.")

;; (defun rk/copilot-disable-predicate ()
;;   "When copilot should not automatically show completions."
;;   (or rk/copilot-manual-mode
;;       (member major-mode rk/no-copilot-modes)
;;       (company--active-p)))

;; (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

;;    (use-package company-tabnine
;;      :ensure t)
;; (add-to-list 'company-backends #'company-tabnine)

(use-package smartparens
  :config (smartparens-global-mode))

;; (use-package eaf
;;   :load-path "~/newemacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)) ;; unbind, see more in the Wiki
;; (require 'eaf-music-player)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-image-viewer)
;; (require 'eaf-video-player)

;; (add-to-list 'load-path "~/newemacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-browser)
;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
  ;;(set-frame-parameter (selected-frame) 'alpha <both>)
  (set-frame-parameter (selected-frame) 'alpha '(87 . 60))
  (add-to-list 'default-frame-alist '(alpha . (87 . 60)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(87 . 60) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(setq
 browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 shr-indentation 2                           ; Left-side margin
 shr-width 110                                ; Fold text to 110 columns
 eww-search-prefix "https://duckduckgo.com/?q=")    ; Use another engine for searching

(use-package pocket-reader)

(use-package mu4e
  :ensure disabled
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :config

  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-maildir "~/Maildir")

  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder "/[Gmail].Sent Mail")
  (setq mu4e-refile-folder "/[Gmail].All Mail")
  (setq mu4e-trash-folder "/[Gmail].Trash")

  (setq mu4e-maildir-shortcuts
	'(("/Inbox"                            . ?i)
	  ("/[Gmail].Sent Mail" . ?s)
	  ("/[Gmail].Drafts" . ?d)
	  ("/[Gmail].Trash" . ?t)
	  ("/[Gmail].All Mail" . ?a)))
  )
