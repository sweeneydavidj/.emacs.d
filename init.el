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
  (evil-set-undo-system 'undo-redo)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Adapted from spacemacs funcs.el
;; which was adapted from...
;; https://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun dsw-find-user-init-file ()
  "Edit the User's init file in the current window"
  (interactive)
  (find-file-existing user-init-file))

;; To see more about intercept keymaps see...
;; https://github.com/emacs-evil/evil-collection#key-translation
;; https://github.com/syl20bnr/spacemacs/wiki/Keymaps-guide
(defvar dsw-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode dsw-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(dsw-intercept-mode)

(evil-make-intercept-map dsw-intercept-mode-map 'normal)

(setq dsw-buffer-map (make-sparse-keymap))
(define-key dsw-buffer-map "b" 'helm-buffers-list)
(define-key dsw-buffer-map "n" 'next-buffer)
(define-key dsw-buffer-map "p" 'previous-buffer)
(define-key dsw-buffer-map "d" 'kill-current-buffer)

(setq dsw-file-map (make-sparse-keymap))
(define-key dsw-file-map "f" 'helm-find-files)
(define-key dsw-file-map "i" 'dsw-find-user-init-file)
(define-key dsw-file-map "j" 'dired-jump)

;; In order to get the prefix key text in which-key see
;; https://github.com/justbur/emacs-which-key#keymap-based-replacement
(evil-define-key 'normal dsw-intercept-mode-map (kbd "SPC b") (cons "buffer" dsw-buffer-map))
(evil-define-key 'normal dsw-intercept-mode-map (kbd "SPC f") (cons "file" dsw-file-map))
