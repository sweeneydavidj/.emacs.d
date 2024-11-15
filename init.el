;;; PACKAGE --- Some summary

;;; Commentary:
;;; some commentary

;;; Code:

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (package-refresh-contents)

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package))

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
;; Copy and move files netween dired buffers
(setq dired-dwim-target t)

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Set anyway, but this is actually the default
(set-frame-font "UbuntuMono-14" nil t)

(recentf-mode 1)

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
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package rg)

(use-package magit
  :pin melpa-stable
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package expreg
  :bind(
        ("C-'" . expreg-expand)
        ("C-\"" . expreg-contract)))

(use-package jump-char
  :bind(
        "C-;" . jump-char-forward))

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

(setq js-indent-level 2)

(use-package web-mode
  :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-block-padding 2))

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
  :repeat (:enter (git-gutter:next-hunk git-gutter:previous-hunk) :exit (magit-status))
  "n" #'git-gutter:next-hunk
  "p" #'git-gutter:previous-hunk
  "s" #'magit-status
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

;;; init.el ends here
