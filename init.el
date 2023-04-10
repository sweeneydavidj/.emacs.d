;;; package --- Some summary

;;; Commentary:
;;; some commentary

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;;TODO straight.el package manager ?

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; There are three different functions that tend to clutter the directory tree.
;; backup, autosave and file locks
;; We keep the functionality but just move them out of the way.
;;
;; Autosave can be customised the same way as lockfiles below
;; using the variable auto-save-file-name-transforms, but by default
;; it is set to the system /tmp directory.
(setq backup-directory-alist `(("." . "~/tmp/emacs-backups/")))
(setq lock-file-name-transforms `((".*" "~/tmp/emacs-lockfiles/" t)))

(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
;; When format or update from Git outside of Emacs
;; then reload changed buffers
(global-auto-revert-mode 1)

;; https://github.com/syl20bnr/spacemacs/issues/5070
(setq-default evil-kill-on-visual-paste nil)

(setq scroll-conservatively 101)
(setq scroll-margin 5)
(setq column-number-mode t)
(setq recentf-max-saved-items 100)

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Set anyway, but this is actually the default
(set-frame-font "UbuntuMono-15" nil t)

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

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package company
  :config
  (global-company-mode 1))

(use-package projectile
  :config
  (projectile-mode 1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x)
  )

(use-package helm-ag)

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode 1))

(use-package magit
  :pin melpa-stable
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package smartparens
  :config
  (smartparens-global-mode))

(require 'treesit)

(use-package elixir-mode)

;; https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/19196/161
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored "artifacts$")
  (add-to-list 'lsp-file-watch-ignored "assets$")
  (add-to-list 'lsp-file-watch-ignored "_build$")
  (add-to-list 'lsp-file-watch-ignored "chemo_engine$")
  (add-to-list 'lsp-file-watch-ignored "strat_calc$")
  (add-to-list 'lsp-file-watch-ignored "chemo_calc$")
  (add-to-list 'lsp-file-watch-ignored "reflex$")
  (add-to-list 'lsp-file-watch-ignored "reflex_viz$")
  (add-to-list 'lsp-file-watch-ignored "deps$")
  (add-to-list 'lsp-file-watch-ignored "docker$")
  (add-to-list 'lsp-file-watch-ignored "docs$")
  (add-to-list 'lsp-file-watch-ignored ".elixir_ls$")
  (add-to-list 'lsp-file-watch-ignored "priv/static$")
  )

(use-package flycheck
  :init
  (global-flycheck-mode))

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; (package-refresh-contents)
(use-package lsp-mode
  :init
  (add-to-list 'exec-path "/opt/elixir-ls/release")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (elixir-mode . lsp)
  :commands lsp)

(use-package web-mode
  :mode ("\\.heex\\'" . web-mode)
  :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-block-padding 2))

;; Make sure Emacs has support for dynamic modules...
;; C-h v system-configuration-options
;; value should include --with-modules
;; Install necessary packages...
;; sudo apt install cmake libtool libtool-bin
;; (use-package vterm)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-nerd-commenter)

;; Adapted from spacemacs funcs.el
;; which was adapted from...
;; https://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun dsw-find-user-init-file ()
  "Edit the User's init file in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun dsw-switch-scratch-bufer ()
  "Edit the User's init file in the current window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(setq dsw-buffer-map (make-sparse-keymap))
(define-key dsw-buffer-map "b" 'helm-buffers-list)
(define-key dsw-buffer-map "n" 'next-buffer)
(define-key dsw-buffer-map "p" 'previous-buffer)
(define-key dsw-buffer-map "d" 'kill-current-buffer)
(define-key dsw-buffer-map "s" 'dsw-switch-scratch-bufer)

(setq dsw-comment-map (make-sparse-keymap))
(define-key dsw-comment-map "l" 'evilnc-comment-or-uncomment-lines)
(define-key dsw-comment-map "p" 'evilnc-comment-or-uncomment-paragraphs)

(setq dsw-file-map (make-sparse-keymap))
(define-key dsw-file-map "f" 'helm-find-files)
(define-key dsw-file-map "i" 'dsw-find-user-init-file)
(define-key dsw-file-map "j" 'dired-jump)
(define-key dsw-file-map "r" 'helm-recentf)

(setq dsw-magit-map (make-sparse-keymap))
(define-key dsw-magit-map "s" 'magit-status)

(setq dsw-help-map (make-sparse-keymap))
(define-key dsw-help-map "l" 'find-library)

(setq dsw-jump-map (make-sparse-keymap))
(define-key dsw-jump-map "b" 'xref-pop-marker-stack)
(define-key dsw-jump-map "d" 'lsp-find-definition)
(define-key dsw-jump-map "i" 'lsp-find-implementation)
(define-key dsw-jump-map "r" 'lsp-find-references)

(setq dsw-project-map (make-sparse-keymap))
(define-key dsw-project-map "f" 'projectile-find-file)

(setq dsw-window-map (make-sparse-keymap))
(define-key dsw-window-map "d" 'evil-window-delete)
(define-key dsw-window-map "m" 'delete-other-windows) ;; maximize
(define-key dsw-window-map "h" 'evil-window-left)
(define-key dsw-window-map "l" 'evil-window-right)
(define-key dsw-window-map "j" 'evil-window-down)
(define-key dsw-window-map "k" 'evil-window-up)
(define-key dsw-window-map "s" 'evil-window-split)
(define-key dsw-window-map "v" 'evil-window-vsplit)

(use-package bind-map)

(bind-map dsw-base-leader-map
  :keys ("C-SPC")
  :evil-keys ("SPC")
  :evil-states (normal motion visual)
  :override-minor-modes t)

;; See the Keymaps hierarchy guide here...
;; https://github.com/syl20bnr/spacemacs/wiki/Keymaps-guide
;;
;; In order to get the prefix key text in which-key see
;; https://github.com/justbur/emacs-which-key#keymap-based-replacement

(bind-map-set-keys dsw-base-leader-map
  "b" (cons "buffer" dsw-buffer-map)
  "c" (cons "comment" dsw-comment-map)
  "f" (cons "file" dsw-file-map)
  "g" (cons "magit" dsw-magit-map)
  "h" (cons "help" dsw-help-map)
  "j" (cons "jump" dsw-jump-map)
  "p" (cons "project" dsw-project-map)
  "w" (cons "window" dsw-window-map)
  "/" (cons "search project" 'helm-projectile-ag)
  "SPC" (cons "M-x" 'helm-M-x))

(put 'dired-find-alternate-file 'disabled nil)

(defun dsw-dired-up-directory ()
  "Take Dired up one directory, but behave like `dired-find-alternate-file`."
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))

;; For inspiration see the answer here...
;; https://emacs.stackexchange.com/questions/26450/how-to-remap-to-in-evil-mode
(with-eval-after-load 'dired
 (evil-define-key 'normal dired-mode-map (kbd "h") 'dsw-dired-up-directory)
 (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-alternate-file)
  )

(eval-after-load 'elixir
  (evil-define-key 'normal elixir-mode-map (kbd "SPC =") (cons "format" 'lsp-format-buffer))
  )

;;; init.el ends here
