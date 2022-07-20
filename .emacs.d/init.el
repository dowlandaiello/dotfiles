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
(use-package swiper
  :ensure t)
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil))
(use-package ivy
  :ensure t
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
  :ensure t
  :config
  (ivy-rich-mode 1))
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-laserwave t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (custom-set-faces
   `(ivy-current-match ((t (:background ,(doom-color 'region)))))))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 40))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t
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
(use-package whitespace
  :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
(use-package org
  :ensure t)

;; Modes
(use-package rust-mode
  :ensure t
  :init
  (autoload 'rust-mode "rust-mode" nil t))
(use-package haskell-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)

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
(window-divider-mode +1)
(setq window-divider-default-right-width 2 window-divider-default-bottom-width 2)

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
       '(("\\.rs\\'" . rust-mode) ("\\.rasi\\'" . prog-mode)) auto-mode-alist))
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
                            (tree-sitter-hl-mode)
                            (disable-tabs)))
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Custom controls
(delete-selection-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy-rich yaml-mode which-key rainbow-delimiters tree-sitter-langs tree-sitter doom-themes rust-mode all-the-icons doom-modeline counsel swiper use-package ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
