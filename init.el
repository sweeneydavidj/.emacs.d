;;; PACKAGE --- Some summary

;;; Commentary:
;;; some commentary

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (package-refresh-contents)

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package))

(define-key global-map (kbd "<escape>") 'keyboard-escape-quit)

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

(setq scroll-conservatively 101)
(setq scroll-margin 3)
(setq isearch-lazy-count t)
(setq isearch-lazy-highlight t)
(setq column-number-mode t)
(setq recentf-max-saved-items 100)
(setq-default indent-tabs-mode nil)

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Set anyway, but this is actually the default
(set-frame-font "UbuntuMono-14" nil t)

(recentf-mode 1)

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

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

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

(use-package smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

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

(use-package evil-nerd-commenter)

(defun dsw-find-user-init-file ()
  "Edit the User's init file in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun dsw-switch-scratch-bufer ()
  "Edit the User's init file in the current window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq dsw-buffer-map (make-sparse-keymap))
(define-key dsw-buffer-map "b" 'switch-to-buffer)
(define-key dsw-buffer-map "B" 'switch-to-buffer-other-window)
(define-key dsw-buffer-map "n" 'next-buffer)
(define-key dsw-buffer-map "o" 'mode-line-other-buffer)
(define-key dsw-buffer-map "p" 'previous-buffer)
(define-key dsw-buffer-map "d" 'kill-current-buffer)
(define-key dsw-buffer-map "k" 'kill-buffer)
(define-key dsw-buffer-map "s" 'dsw-switch-scratch-bufer)

(setq dsw-comment-map (make-sparse-keymap))
(define-key dsw-comment-map "l" 'evilnc-comment-or-uncomment-lines)
(define-key dsw-comment-map "p" 'evilnc-comment-or-uncomment-paragraphs)

(setq dsw-file-map (make-sparse-keymap))
(define-key dsw-file-map "f" 'find-file)
(define-key dsw-file-map "i" 'dsw-find-user-init-file)
(define-key dsw-file-map "j" 'dired-jump)
(define-key dsw-file-map "r" 'recentf)

(setq dsw-magit-map (make-sparse-keymap))
(define-key dsw-magit-map "s" 'magit-status)

(setq dsw-help-map (make-sparse-keymap))
(define-key dsw-help-map "l" 'find-library)
(define-key dsw-help-map "h" 'eldoc)

(setq dsw-jump-map (make-sparse-keymap))
(define-key dsw-jump-map "b" 'xref-pop-marker-stack)

(setq dsw-window-map (make-sparse-keymap))
(define-key dsw-window-map "e" 'balance-windows)
(define-key dsw-window-map "d" 'delete-window)
(define-key dsw-window-map "m" 'delete-other-windows) ;; maximize
(define-key dsw-window-map "s" 'split-window-below)
(define-key dsw-window-map "v" 'split-window-right)

(setq dsw-fly-map (make-sparse-keymap))
(define-key dsw-fly-map "b" 'flymake-show-buffer-diagnostics)
(define-key dsw-fly-map "p" 'flymake-show-project-diagnostics)

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Copy and move files netween dired buffers
(setq dired-dwim-target t)

(defun dsw-dired-up-directory ()
  "Take Dired up one directory, but behave like `dired-find-alternate-file`."
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(with-eval-after-load 'dired
 (define-key dired-mode-map (kbd "C-6") 'dsw-dired-up-directory))

(define-key global-map (kbd "C-c b") (cons "buffer" dsw-buffer-map))
(define-key global-map (kbd "C-c c") (cons "comment" dsw-comment-map))
(define-key global-map (kbd "C-c f") (cons "file" dsw-file-map))
(define-key global-map (kbd "C-c g") (cons "magit" dsw-magit-map))
(define-key global-map (kbd "C-c h") (cons "help" dsw-help-map))
(define-key global-map (kbd "C-c j") (cons "jump" dsw-jump-map))
(define-key global-map (kbd "C-c w") (cons "window" dsw-window-map))
(define-key global-map (kbd "C-c y") (cons "fly" dsw-fly-map))
(define-key global-map (kbd "C-c /") (cons "search project" 'project-find-regexp))

(put 'narrow-to-region 'disabled nil)

;; https://www.reddit.com/r/emacs/comments/10l40yi/comment/j5usxmr/
(defun +keyboard-escape-quit-adv (fun)
"Around advice for `keyboard-escape-quit' FUN.
 Preserve window configuration when pressing ESC."
(let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
  (funcall fun)))
(advice-add #'keyboard-escape-quit :around #'+keyboard-escape-quit-adv)

;;; init.el ends here
