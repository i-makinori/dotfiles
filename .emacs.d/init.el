
;;;; init

(setq ring-bell-function 'ignore) ;;bell
(setq make-backup-files nil) ;;file
(setq auto-save-default nil)
(set-language-environment 'Japanese) ;; encode
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil) ;; indent

(setq inhibit-startup-screen t) ;; ignore startup
(show-paren-mode t) ;; paren
(menu-bar-mode 0) ;; window-bars
(tool-bar-mode 0)

(global-set-key (kbd "C-h") 'delete-backward-char) ;; keys
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-n") 'scroll-up)

;; scroll

(setq scroll-margin 1)
(setq scroll-conservatively 1)
(setq next-screen-context-lines 10)

;;;; theme

(defun use-default-theme ()
  (load-theme 'manoj-fix t))
(use-default-theme)

(set-frame-font "ricty-13")

(setq-default show-trailing-whitespace t) ;; highlight
(add-hook 'font-lock-mode-hook            ;; highlight
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\t" 0 'trailing-whitespace prepend)))))



;;;; package
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;;;; helm
(require 'helm)
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x) ;; key bindings
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)


;;;; sudo open file
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; shell
(setq shell-command-switch "-ic")

(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))


;;;; modeline
(require 'smart-mode-line)

(setq sml/active-background-color "gray60")

(column-number-mode t)
(line-number-mode t)

(setq sml/read-only-char "%%")
(setq sml/modified-char "*")

(setq sml/hidden-modes '(" Helm" " AC"))
(setq sml/extra-filler -10)

(add-to-list 'sml/replacer-regexp-list '("^.+/junk/[0-9]+/" ":J:") t)

(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'light)

;;;; orgmode


;; company
;; (autoload 'company)
;; (autoload 'company-quickhelp)
(with-eval-after-load 'company
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 0.35)
  (setq company-auto-expand t)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer)

  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
  )

;;;; haskell

;; hook
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
;; haskell script
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))
;; indent
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;;



;;;; lisp
(setq inferior-lisp-program "ros run")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))




;;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a25f3d016f2d56edcfa77ccf6c76fa3b76db349a02233916146c3a837442bdca" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "f588a59b242302fa8aca00c214d5ebccf385c4930965f8b7783649b51e369e08" default)))
 '(package-selected-packages
   (quote
    (slime markdown-mode+ magit total-lines smart-mode-line popwin magit-popup helm haskell-mode git-commit ghub diminish company-quickhelp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
