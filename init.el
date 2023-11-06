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

;; https://github.com/syl20bnr/spacemacs/issues/5070
(setq-default evil-kill-on-visual-paste nil)

(setq scroll-conservatively 101)
(setq scroll-margin 3)
(setq column-number-mode t)
(setq recentf-max-saved-items 100)
(setq-default indent-tabs-mode nil)

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Set anyway, but this is actually the default
(set-frame-font "UbuntuMono-15" nil t)

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

(setq js-indent-level 2)

(use-package web-mode
  ;; :mode ("\\.heex\\'" . web-mode)
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
  (setq highlight-indent-guides-auto-top-character-face-perc 75)
  )

;; Make sure Emacs has support for dynamic modules...
;; C-h v system-configuration-options
;; value should include --with-modules
;; Install necessary packages...
;; sudo apt install cmake libtool libtool-bin
;; (use-package vterm)

(use-package evil
  :init
  ;; (setq evil-want-integration t)
  ;; (setq evil-want-keybinding nil)
  ;; (setq evil-search-module 'evil-search)
  :config
  ;; (evil-mode 1)
  ;; (evil-set-undo-system 'undo-redo)
  )

;; (use-package evil-collection
;;   :after evil
;;   :diminish evil-collection-unimpaired-mode
;;   :config
;;   (evil-collection-init))

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

(define-key evil-motion-state-map (kbd "gb") 'xref-go-back)

;; TODO review the second last comment (from lawlist) and maybe refactor below
;; Probably good to read elisp chapter on Functions first
;; https://www.reddit.com/r/emacs/comments/3ytb6n/a_better_way_to_define_a_new_prefix/

(setq dsw-buffer-map (make-sparse-keymap))
(define-key dsw-buffer-map "b" 'switch-to-buffer)
(define-key dsw-buffer-map "B" 'switch-to-buffer-other-window)
(define-key dsw-buffer-map "n" 'next-buffer)
(define-key dsw-buffer-map "o" 'mode-line-other-buffer)
(define-key dsw-buffer-map "p" 'previous-buffer)
(define-key dsw-buffer-map "d" 'kill-current-buffer)
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
(define-key dsw-window-map "d" 'evil-window-delete)
(define-key dsw-window-map "m" 'delete-other-windows) ;; maximize
(define-key dsw-window-map "h" 'evil-window-left)
(define-key dsw-window-map "l" 'evil-window-right)
(define-key dsw-window-map "j" 'evil-window-down)
(define-key dsw-window-map "k" 'evil-window-up)
(define-key dsw-window-map "s" 'evil-window-split)
(define-key dsw-window-map "v" 'evil-window-vsplit)

(setq dsw-fly-map (make-sparse-keymap))
(define-key dsw-fly-map "b" 'flymake-show-buffer-diagnostics)
(define-key dsw-fly-map "p" 'flymake-show-project-diagnostics)

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
  "w" (cons "window" dsw-window-map)
  "y" (cons "fly" dsw-fly-map)
  "/" (cons "search project" 'project-find-regexp)
  "SPC" (cons "M-x" 'execute-extended-command)
  )

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

;; For inspiration see the answer here...
;; https://emacs.stackexchange.com/questions/26450/how-to-remap-to-in-evil-mode
(with-eval-after-load 'dired
 (evil-define-key 'normal dired-mode-map (kbd "h") 'dsw-dired-up-directory)
 (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-alternate-file)
 (define-key dired-mode-map (kbd "C-<return>") 'dired-find-alternate-file)
 (define-key dired-mode-map (kbd "C-6") 'dsw-dired-up-directory))

(eval-after-load 'elixir-ts
  (evil-define-key 'normal elixir-ts-mode-map (kbd "SPC =") (cons "format" 'eglot-format-buffer)))

(eval-after-load 'heex-ts
  (evil-define-key 'normal heex-ts-mode-map (kbd "SPC =") (cons "format" 'eglot-format-buffer)))

(setq-default cursor-type 'bar)
;; http://makble.com/how-to-toggle-evil-mode-in-emacs
(defun toggle-evil-local-mode ()
  (interactive)
  (if (bound-and-true-p evil-local-mode)
    (progn
      (turn-off-evil-mode)
      (setq cursor-type 'bar)
    )
    (progn
      (turn-on-evil-mode)
      (setq cursor-type 'box)
    )
  )
)
 
;; (add-hook 'elixir-ts-mode-hook 'evil-local-mode)
(add-hook 'heex-ts-mode-hook 'evil-local-mode)
(add-hook 'prog-mode-hook 'evil-local-mode)

(define-key global-map (kbd "M-u") 'toggle-evil-local-mode)
(define-key global-map (kbd "C-c f") (cons "file"  dsw-file-map))
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
