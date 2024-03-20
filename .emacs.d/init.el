(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
; defaults write org.gnu.Emacs HideDocumentIcon YES

(setq custom-file "~/.emacs.d/custom-set-vars.el")

(let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

(let ((backup-dir (expand-file-name (concat user-emacs-directory "backups/"))))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))))


(global-display-line-numbers-mode t)

(setq treesit-font-lock-level 4)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key global-map (kbd "C-c q") 'delete-window)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :ensure t
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
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(defun my/projectile-project-root ()
  "Identify the project root for npm projects and fall back to default methods."
  (or
   ;; First, check for package.json in the current directory and upwards.
   (projectile-locate-dominating-file default-directory "package.json")
   ;; If not found, fall back to Projectile's default project root detection.
   (projectile-project-root)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
					;(setq projectile-project-root-function 'my/projectile-project-root)
  :config
  (projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json")
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))



(use-package counsel-projectile
  :ensure t)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-items '((recents   . 5)
                        (projects  . 20)))
  :config
  (dashboard-setup-startup-hook))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :ensure t)

(defhydra hydra-manage-windows (:color red)
  "manage windows"
  ("s" shrink-window-horizontally "shrink horizontally" :column "Sizing")
    ("e" enlarge-window-horizontally "enlarge horizontally")
    ("b" balance-windows "balance window height")
    ("m" maximize-window "maximize current window")
    ("M" minimize-window "minimize current window")
    
    ("H" split-window-below "split horizontally" :column "Split management")
    ("v" split-window-right "split vertically")
    ("d" delete-window "delete current window")
    ("x" delete-other-windows "delete-other-windows")

    ("h" windmove-left "← window" :color blue :column "Navigation")
    ("j" windmove-down "↓ window")
    ("k" windmove-up "↑ window")
    ("l" windmove-right "→ window")
    ("q" nil "quit menu" :color blue :column nil))

(global-set-key (kbd "C-c C-j") 'hydra-manage-windows/body)

(use-package ripgrep
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (
         (js-mode . lsp)
	 (typescript-mode . lsp)
	 (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point))

(use-package company
  :ensure t
  :bind
  ("C-c C-c" . company-complete)
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay nil))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-counsel-switch-buffer)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package vterm
  :ensure t)

(defun open-new-vterm ()
  "open a new vterm instance"
  (interactive)
  (let ((buffer (generate-new-buffer-name "*vterm*")))
    (vterm buffer)))

(global-set-key (kbd "C-c C-v") 'open-new-vterm)

(setq major-mode-remap-alist
 '((javascript-mode . js-ts-mode)
   (typescript-mode . js-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (java-mode . java-ts-mode)))

(use-package yasnippet
  :ensure t
  :bind ("C-c y" . company-yasnippet)
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
