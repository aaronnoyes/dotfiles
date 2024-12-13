(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)
(global-display-line-numbers-mode t)

(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)

(dolist (dir '("backups" "auto-save" "locks"))
  (let ((path (expand-file-name dir user-emacs-directory)))
    (unless (file-directory-p path)
      (make-directory path t))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq lock-file-name-transforms
      `((".*" ,(expand-file-name "locks/" user-emacs-directory) t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(perspective rainbow-delimiters lsp-mode flycheck consult-projectile projectile-ripgrep projectile ripgrep consult vterm which-key vertico spacemacs-theme orderless marginalia lua-mode corfu cape)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package spacemacs-theme
  :ensure t
  :config
  (load-theme 'spacemacs-dark :noconfimr))

;; tree-sitter
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
;; https://github.com/casouri/tree-sitter-module/releases
;; grammars go in ~/.emacs.d/tree-sitter

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (setq corfu-popupinfo-delay 0.2)
  (corfu-popupinfo-mode))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package vterm
  :ensure t
  :config
  (defun my/vterm-new ()
    "Create a new vterm buffer with a unique name."
    (interactive)
    (let ((buffer (generate-new-buffer-name "vterm")))
      (vterm buffer)))
  :bind
  (("C-c t" . my/vterm-new)))

(use-package consult
  :ensure t
  :config
  ;; Define a prefix map for Consult
  (define-prefix-command 'my-consult-map)
  (global-set-key (kbd "C-c C-c") 'my-consult-map)

  ;; Keybindings within the prefix map
  (define-key my-consult-map (kbd "l") 'consult-line)
  (define-key my-consult-map (kbd "b") 'consult-buffer))

(use-package ripgrep
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

(use-package consult-projectile
  :ensure t)

(use-package projectile-ripgrep
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((prog-mode . lsp)  ;; Automatically start LSP in any programming mode
         (lsp-mode . lsp-enable-which-key-integration)
         (emacs-lisp-mode . (lambda () (lsp-mode -1))))  ;; Disable LSP in emacs-lisp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-prefer-flymake nil))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)  ;; Enable flycheck globally
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-idle-change-delay 2))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c c"))
  :init
  (persp-mode))
