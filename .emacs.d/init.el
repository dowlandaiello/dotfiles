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
  :ensure t)
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
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t
  :config
  (tree-sitter-require 'rust))
(use-package whitespace
  :ensure t)

;; Modes
(use-package rust-mode
  :ensure t
  :init
  (autoload 'rust-mode "rust-mode" nil t))

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
      (append '(("\\.rs\\'" . rust-mode)) auto-mode-alist))
(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode)
                            (line-number-mode)
                            (enable-tabs)
                            (setq display-line-numbers 'relative)
                            (setq whitespace-style '(face tabs tab-mark trailing))))
(add-hook 'rust-mode-hook (lambda ()
                            (tree-sitter-hl-mode)))
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter doom-themes rust-mode all-the-icons doom-modeline counsel swiper use-package ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
