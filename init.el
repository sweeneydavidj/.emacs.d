;;; PACKAGE --- Some summary

;;; Commentary:
;;; some commentary

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (package-refresh-contents)

(require 'use-package)
(setq use-package-always-ensure t)

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

(scroll-bar-mode 1)
(tool-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

(repeat-mode 1)
(setq repeat-exit-timeout 3)

(winner-mode 1)

(setq-default cursor-type 'bar)
(setq scroll-conservatively 101)
(setq scroll-margin 3)
(setq isearch-lazy-count t)
(setq isearch-lazy-highlight t)
(setq column-number-mode t)
(setq recentf-max-saved-items 100)
(setq-default indent-tabs-mode nil)
(setq help-window-select t)
(setq set-mark-command-repeat-pop t)
(setq bookmark-save-flag 1)
;; Copy and move files between dired buffers
(setq dired-dwim-target t)

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(setq sql-connection-alist
      '((marko_dev (sql-product 'postgres)
                   (sql-user "postgres")
                   (sql-server "localhost")
                   (sql-database "localhost")
                   (sql-database "postgres://postgres:postgres@localhost/marko_dev"))))

(set-face-attribute 'default nil :height 135)

(recentf-mode 1)
(delete-selection-mode 1)

(setq savehist-additional-variables '(register-alist))
(savehist-mode 1)

(use-package diminish)

(use-package doom-themes
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

(use-package keycast)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package rg)

(use-package magit
  :pin melpa-stable
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package expreg
  :bind(
        ("C-;" . expreg-expand)
        ("C-:" . expreg-contract)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-'") nil))

(use-package surround
  :bind-keymap ("C-'" . surround-keymap))

(use-package eat)
(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(require 'treesit)
(require 'heex-ts-mode)
(require 'elixir-ts-mode)

;; elixir-mode, elixir-ts-mode, heex-ts-mode
;; are setup in the eglot-server-programs variable to look for
;; language_server.sh
;; so just add that to the path in ~/.profile using...
;; PATH="/opt/elixir-ls/release:$PATH"
(require 'eglot)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(add-hook 'python-ts-mode-hook 'eglot-ensure)

(require 'yaml-ts-mode)

;; https://github.com/camdencheek/tree-sitter-dockerfile
(require 'dockerfile-ts-mode)

(setq js-indent-level 2)

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 35)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-auto-top-character-face-perc 75))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.5)
  (setq git-gutter:added-sign "|")
  (setq git-gutter:modified-sign "|")
  (setq git-gutter:lighter " GG"))

(use-package evil-nerd-commenter)

(use-package ox-hugo
  :after ox)

(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

(defun dsw-mix-format()
  "Elixir's mix format"
  (interactive)
  (let ((default-directory (vc-root-dir)))
    (async-shell-command "mix format")))

;; (defun dsw-after-save-actions()
;;   (cond ((or (eq major-mode 'elixir-ts-mode)
;;              (eq major-mode 'heex-ts-mode))
;;          (dsw-mix-format))))

;; (add-hook 'after-save-hook 'dsw-after-save-actions)

(defun dsw-find-user-init-file ()
  "Open the User's init file in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun dsw-switch-scratch-bufer ()
  "Open the scatch buffer in the current window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun dsw-dired-up-directory ()
  "Take Dired up one directory, but behave like `dired-find-alternate-file`."
  (interactive)
  (find-alternate-file ".."))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "^" #'dsw-dired-up-directory))

(defvar-keymap dsw-buffer-map
  "B" #'switch-to-buffer-other-window
  "n" #'next-buffer
  "p" #'previous-buffer
  "d" #'kill-current-buffer
  "s" #'dsw-switch-scratch-bufer)

(defvar-keymap dsw-comment-map
  "l" #'evilnc-comment-or-uncomment-lines
  "p" #'evilnc-comment-or-uncomment-paragraphs)

(defvar-keymap dsw-file-map
  "i" #'dsw-find-user-init-file
  "r" #'recentf)

(defvar-keymap dsw-git-map
  :repeat (:enter (git-gutter:next-hunk git-gutter:previous-hunk))
  "n" #'git-gutter:next-hunk
  "p" #'git-gutter:previous-hunk
  )

(defvar-keymap dsw-help-map
  "l" #'find-library
  "h" #'eldoc)

(defvar-keymap dsw-window-map
  "e" #'balance-windows
  "d" #'delete-window
  "r" #'winner-redo
  "u" #'winner-undo
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "H" #'windmove-swap-states-left
  "J" #'windmove-swap-states-down
  "K" #'windmove-swap-states-up
  "L" #'windmove-swap-states-right)

(defvar-keymap dsw-fly-map
  "b" #'flymake-show-buffer-diagnostics
  "p" #'flymake-show-project-diagnostics)

(keymap-global-set "C-c b" (cons "buffer" dsw-buffer-map))
(keymap-global-set "C-c c" (cons "comment" dsw-comment-map))
(keymap-global-set "C-c f" (cons "file" dsw-file-map))
(keymap-global-set "C-c g" (cons "git" dsw-git-map))
(keymap-global-set "C-c h" (cons "help" dsw-help-map))
(keymap-global-set "C-c w" (cons "window" dsw-window-map))
(keymap-global-set "C-c y" (cons "fly" dsw-fly-map))

(setq frame-title-format
      '(buffer-file-name "%f" "%b"))

;;; init.el ends here
