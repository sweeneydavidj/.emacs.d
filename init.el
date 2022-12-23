(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;TODO straight.el package manager ?

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Not working ???
;;(setq backup-directory-alist `(("." . "~/tmp/emacs-backup/")))

(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :config
  (solaire-global-mode 1))

(use-package bind-map)

(bind-map my-file-map
  :evil-keys ("SPC")
  :evil-states (normal motion visual))

(bind-map-set-keys my-file-map
  "fj" 'dired-jump
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package company
  :config
  (company-mode 1))

(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x)
  )

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(with-eval-after-load 'dired
  (define-key evil-motion-state-map (kbd "h") 'my-dired-up-directory)
  (define-key evil-motion-state-map (kbd "l")  'dired-find-alternate-file))
(put 'dired-find-alternate-file 'disabled nil)
