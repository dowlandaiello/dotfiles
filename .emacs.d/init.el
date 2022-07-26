;--------------------------------;
; File: ~/.config/emacs/init.el  ;
;                                ;
; Desc: My stock emacs config.   ;
; No frills, no doom, no space.  ;
; Just normal emacs!             ;
;                                ;
; Enjoy! <3 ~ Dowland            ;
;--------------------------------;

;; Use MELPA repos for packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install the use-package package for package management
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Packages that I use

;; Auto-completion for buffer switching, file finding, searching
;; This package can be split depending on what repo you find it from
(use-package swiper)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil))
(use-package ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("C-f" . ivy-done)
         ("C-k" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
         ("C-k" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package ivy-rich
  :config
  (ivy-rich-mode 1))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-laserwave t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (custom-set-faces
   `(ivy-current-match ((t (:background ,(doom-color 'region)))))))
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 40))
(use-package all-the-icons
  :if (display-graphic-p))
(use-package tree-sitter)
(use-package tree-sitter-langs
  :config
  (tree-sitter-require 'rust)
  (tree-sitter-require 'json)
  (tree-sitter-require 'html)
  (tree-sitter-require 'tsx)
  (tree-sitter-require 'python)
  (tree-sitter-require 'go)
  (tree-sitter-require 'c)
  (tree-sitter-require 'bash)
  (tree-sitter-require 'css)
  (tree-sitter-require 'javascript)
  (tree-sitter-require 'java)
  (tree-sitter-require 'typescript))
(use-package whitespace)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
(use-package magit)
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

;; Shell settings
(defun config-term ()
  (local-set-key (kbd "M-n") 'vterm-next-prompt)
  (local-set-key (kbd "M-p") 'vterm-previous-prompt))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh"))
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  :hook (vterm-mode . config-term))

;; Modes
(use-package rust-mode
  :init
  (autoload 'rust-mode "rust-mode" nil t)
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred))
(use-package haskell-mode
  :hook (haskell-mode . lsp-deferred))
(use-package yaml-mode)
(use-package json-mode)
(use-package hardcore-mode
  :ensure
  :config (global-hardcore-mode))
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
  :config (counsel-projectile-mode))
(use-package markdown-mode)
(use-package editorconfig
  :config (editorconfig-mode 1))
(use-package prettier-js
  :hook ((web-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

;; Web dev package
(use-package web-mode)
(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred))
(use-package rjsx-mode
  :mode "\\.js.*$")

;; No home page
(setq inhibit-startup-message t)

;; Remove GUI bloat
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Alert errors
(setq visible-bell t)

;; Visual preferences:
;; - dark theme
;; - bottom padding
;; - font
(set-fringe-mode 10)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 80)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 80)
(set-face-attribute 'variable-pitch nil :font "Roboto":height 90 :weight 'regular)
(window-divider-mode +1)
(setq window-divider-default-right-width 2 window-divider-default-bottom-width 2)

;; Org Mode Config
;; - Use sans-serif, regular fonts for non-programming sections
(defun configure-org ()
  (org-indent-mode)
  (visual-line-mode 1)
  (variable-pitch-mode 1))
(defun stylize-org ()
  (copy-face 'org-level-1 'ivy-org)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Roboto" :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
(defun pad-org ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :hook (org-mode . configure-org)
  :config
  (stylize-org)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link)))
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
(use-package visual-fill-column
  :hook (org-mode . pad-org))

;; Mode-specific configs
;; Specifically, blocking tabs in certain modes, and prefering them over spaces
;; in others
(setq custom-tab-width 4)

(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(setq auto-mode-alist
      (append
       '(("\\.tsx\\'" . web-mode)
         ("\\.rasi\\'" . prog-mode)) auto-mode-alist))
(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode)
                            (line-number-mode)
                            (column-number-mode)
                            (enable-tabs)
                            (setq display-line-numbers 'relative)
                            (setq whitespace-style '(face tabs tab-mark trailing))
                            (setq whitespace-display-mappings
                                  '((tab-mark 9 [124 9] [92 9])))))
(add-hook 'rust-mode-hook (lambda ()
                            (tree-sitter-hl-mode)))
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)
(add-hook 'lisp-data-mode 'disable-tabs)

;; TODO: Come up with a better way to detect NVM binaries
(setq exec-path (append exec-path '("~/.nvm/versions/node/v16.4.0/bin")))

;; C-h backspace, f1 for help
(delete-selection-mode 1)
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key [?\C-j] 'newline-and-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode web-mode company json-mode vterm eterm-256color org-bullets markdown-mode magit counsel-projectile projectile hardcore-mode helpful ivy-rich yaml-mode which-key rainbow-delimiters tree-sitter-langs tree-sitter doom-themes rust-mode all-the-icons doom-modeline counsel swiper use-package ivy))
 '(safe-local-variable-values
   '((eval setq org-publish-project-alist
	   (cons
	    (let
		((based
		  (file-name-as-directory
		   (projectile-project-root))))
	      (list "roadmap" :base-directory
		    (concat based "content")
		    :recursive t :publishing-directory
		    (concat based "public")
		    :index-filename "Status.org" :index-title "Vision Development Roadmap"))
	    (cond
	     ((boundp 'org-publish-project-alist)
	      org-publish-project-alist)
	     (t 'nil))))
     (eval setq org-publish-project-alist
	   (cons
	    (let
		((based
		  (file-name-directory buffer-file-name)))
	      (list "roadmap" :base-directory
		    (concat based "content")
		    :recursive t :publishing-directory
		    (concat based "public")
		    :index-filename "Status.org" :index-title "Vision Development Roadmap"))
	    (cond
	     ((boundp 'org-publish-project-alist)
	      org-publish-project-alist)
	     (t 'nil))))
     (eval setq org-publish-project-alist
	   (cons
	    (let
		((based
		  (file-name-as-directory default-directory)))
	      (list "roadmap" :base-directory
		    (concat based "content")
		    :recursive t :publishing-directory
		    (concat based "public")
		    :index-filename "Status.org" :index-title "Vision Development Roadmap"))
	    (cond
	     ((boundp 'org-publish-project-alist)
	      org-publish-project-alist)
	     (t 'nil)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:background "#4e2e49")))))
