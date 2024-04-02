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

;indenting
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

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

(set-face-attribute 'default nil :font "SauceCodePro Nerd Font Mono")
(setq treesit-font-lock-level 4)

;;custom functions
(defun open-new-vterm ()
  "open a new vterm instance"
  (interactive)
  (let ((buffer (generate-new-buffer-name "*vterm*")))
    (vterm buffer)))

(defun close-user-buffers-and-open-dashboard ()
  "Close all user-opened buffers and open the *dashboard* buffer."
  (interactive)
  (switch-to-buffer "*dashboard*")
  (let ((buffers (buffer-list)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        ;; Check if the buffer's name does not start and end with *, indicating it's a user buffer.
        (unless (or (string-match "^\\*" (buffer-name))
                    (string-match "\\*$" (buffer-name)))
          (kill-buffer buffer))))))


;;keybinds
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key global-map (kbd "C-c q") 'delete-window)
(define-key global-map (kbd "C-c v") 'split-window-right)
(define-key global-map (kbd "C-c h") 'split-window-below)
(define-key global-map (kbd "C-c o") 'other-window)
(define-key global-map (kbd "C-c d") 'delete-other-windows)
(define-key global-map (kbd "C-c t") 'open-new-vterm)
(define-key global-map (kbd "C-c /") 'comment-line)
(define-key global-map (kbd "C-c C-q") 'close-user-buffers-and-open-dashboard)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

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

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-height 25))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-project-root-functions
  '(projectile-root-local
    projectile-root-marked
    projectile-root-top-down
    projectile-root-bottom-up
    projectile-root-top-down-recurring))
    (projectile-switch-project-action #'projectile-dired))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-items '((recents   . 5)
                        (projects  . 20)))
  :config
  (dashboard-setup-startup-hook))

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
  :bind (("C-c g" . magit-status)))

(use-package lsp-java
  :ensure t
  :after lsp
  :config
  (setq lsp-java-save-actions-organize-imports t))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (
         (js-mode . lsp)
         (js-ts-mode . lsp)
	 (typescript-mode . lsp)
	 (typescript-ts-mode . lsp)
	 (java-mode . lsp)
	 (java-ts-mode . lsp)
   (c-mode . lsp)
   (c-ts-mode . lsp)
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

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c b"))
  :init
  (setq persp-state-default-file (expand-file-name ".perspective" user-emacs-directory))
  ;; (add-hook 'kill-emacs-hook #'persp-state-save)
  ;; (if (file-exists-p persp-state-default-file)
  ;;   (persp-state-load persp-state-default-file))
  (persp-mode))

(use-package vterm
  :ensure t)

(global-set-key (kbd "C-c C-v") 'open-new-vterm)

(setq major-mode-remap-alist
 '((javascript-mode . js-ts-mode)
   (typescript-mode . js-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (c-mode . c-ts-mode)
   (java-mode . java-ts-mode)))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; persist history over emacs restarts
;; vertico sorts by history position
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; better directory navigation
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :after (perspective)
  :bind
  ("C-s" . consult-line)
  ("C-c s" . consult-buffer)
  ("C-c :" . consult-goto-line)
  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet-capf
  :ensure t
  :after cape
  :bind
  ("C-c y" . yasnippet-capf)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
