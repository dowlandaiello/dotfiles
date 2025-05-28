{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30;
    extraPackages = epkgs:
      with epkgs; [
        auctex
        modus-themes
        nix-mode
        nix-sandbox
        vterm
        python-mode
        python-black
        lsp-mode
        lsp-ui
        doom-modeline
        all-the-icons
        treesit-grammars.with-all-grammars
        leerzeichen
        rainbow-delimiters
        which-key
        helpful
        editorconfig
        projectile
        magit
        company
        ace-window
        ivy
        swiper
        counsel
        counsel-projectile
        auto-virtualenv
        xclip
        rust-mode
        direnv
        solidity-mode
        web-mode
        typescript-mode
        prettier-js
        lsp-haskell
        haskell-mode
        flycheck
        company-coq
        ace-jump-mode
        poly-R
        ess
        (callPackage ./lean4-mode.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (pkgs.lib) fakeHash;
          inherit (epkgs) melpaBuild compat lsp-mode dash magit-section;
        })
        org-modern
        pdf-tools
      ];
    extraConfig = ''
      ;; Remove GUI bloat
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (menu-bar-mode -1)
      (require 'lean4-mode)
      (require 'ace-jump-mode)

      (load "auctex.el" nil t t)

      (load-theme 'modus-operandi-tinted)
      (setq TeX-auto-save t)
      (setq TeX-parse-self t)

      (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

      (setq gc-cons-threshold (* 100 1024 1024))
      (setq gc-cons-percentage 0.1)

      (add-to-list 'default-frame-alist '(inhibit-double-buffering . nil))

      (setq imagemagick-enabled-types t)
      (setq image-use-external-converter t)

      (setq x-underline-at-descent-line t)
      (setq x-use-underline-position-properties nil)

      (add-to-list 'warning-suppress-log-types '(lsp-mode))
      (add-to-list 'warning-suppress-types '(lsp-mode))

      (defun doom-defer-gc-h ()
        (setq gc-cons-threshold most-positive-fixnum))

      (defun doom-restore-gc-h ()
             (run-at-time 1 nil (lambda () (setq gc-cons-threshold (* 100 1024 1024)))))

      (add-hook 'minibuffer-setup-hook 'doom-defer-gc-h)
      (add-hook 'minibuffer-exit-hook 'doom-restore-gc-h)

      (setq scroll-margin 3)
      (setq scroll-conservatively 100000)
      (setq scroll-preserve-screen-position 1)
      (setq auto-window-vscroll nil)

      (setq fast-but-imprecise-scrolling t)
      (setq jit-lock-defer-time 0)
      (setq redisplay-skip-fontification-on-input t)

      (xclip-mode 1)
      (direnv-mode)

      ;; Alert errors
      (setq visible-bell t)
      (setq inhibit-startup-screen t)
      (setq inhibit-startup-message t)
      (setq initial-scratch-message nil)
      (setq inhibit-splash-screen t)

      (set-fringe-mode 10)
      (set-face-attribute 'default nil :font "Iosevka Nerd Font")
      (set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font")
      (set-face-attribute 'variable-pitch nil :font "Inter")
      (window-divider-mode +1)
      (setq window-divider-default-right-width 2 window-divider-default-bottom-width 2)
      (doom-modeline-mode 1)

      (projectile-mode)
      (counsel-projectile-mode)
      (setq projectile-switch-project-action #'projectile-dired)

      (setq vterm-max-scrollback 10000)
      (setq read-process-output-max (* 1024 1024)) ;; 1mb
      (setq vterm-shell "nu")
      (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "dumb")))
      (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
      (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

      (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
      (add-hook 'haskell-mode-hook (lambda ()
                                   (lsp-deferred)))
      (add-hook 'web-mode-hook (lambda ()
                               (lsp-deferred)
                               (prettier-js-mode)))
      (add-hook 'typescript-mode-hook (lambda ()
                                      (prettier-js-mode)
                                      (lsp-deferred)))
      (add-hook 'python-mode-hook 'lsp)
      (add-hook 'python-mode-hook 'python-black-on-save-mode)
      (add-hook 'rust-mode-hook (lambda ()
        (lsp)))
      (add-hook 'org-mode-hook (lambda ()
                               (setq org-modern-star '("◉" "○" "✸" "✿"))
                               (org-modern-mode)))
      (add-hook 'org-agenda-finalize-hook (lambda ()
                                          (org-modern-agenda)
                                          (setq org-modern-star '("◉" "○" "✸" "✿"))))
      (setq rust-format-on-save t)
      (add-hook 'prog-mode-hook (lambda ()
                                (whitespace-mode)
                                (line-number-mode)
                                (column-number-mode)
                                (setq display-line-numbers 'relative)
                                (setq whitespace-style '(face spaces space-mark tabs tab-mark trailing))
                                (setq whitespace-display-mappings
                                      '((tab-mark 9 [124 9] [92 9])
                                      (space-mark 32 [183] [46])
                                      (space-mark 160 [164] [95])
                                      (newline-mark 10 [36 10])))))

      ;; Flycheck
      (global-flycheck-mode)
      (setq flycheck-command-wrapper-function
              (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command)))
      (setq
            flycheck-executable-find
              (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

      ;; Keybindings
      (delete-selection-mode 1)
      (define-key key-translation-map [?\C-h] [?\C-?])
      (global-set-key [?\C-j] 'newline-and-indent)
      (global-set-key (kbd "M-x") 'counsel-M-x)
      (global-set-key (kbd "C-x C-f") 'counsel-find-file)
      (global-set-key (kbd "M-o") 'ace-window)
      (global-set-key (kbd "C-s") 'swiper)
      (global-set-key (kbd "C-r") 'swiper-backward)
      (global-set-key (kbd "C-c p") 'projectile-command-map)
      (global-set-key (kbd "C-c p e") 'vterm)

      (setq auto-mode-alist
          (append
          '(("\\.tsx\\'" . web-mode)
          ("\\.rasi\\'" . prog-mode)) auto-mode-alist))
      (add-to-list 'auto-mode-alist '("\\.lean\\'" . (lambda ()
                                                     (lean4-mode)
                                                     (whitespace-mode)
                                                     (line-number-mode)
                                                     (column-number-mode)
                                                     (setq display-line-numbers 'relative)
                                                     (setq whitespace-style '(face spaces space-mark tabs tab-mark trailing))
                                                     (setq whitespace-display-mappings
                                                           '((tab-mark 9 [124 9] [92 9])
                                                           (space-mark 32 [183] [46])
                                                           (space-mark 160 [164] [95])
                                                           (newline-mark 10 [36 10])))
                                                     (lsp-mode)
                                                     (global-set-key (kbd "C-c C-i") 'lean4-toggle-info))))

      ;; Buffer stuff
      (recentf-mode 1)
      (savehist-mode 1)
      (winner-mode 1)
      (setq history-length 25)

      ;; Xclip config
      (setq xclip-program "wl-copy")
      (setq xclip-select-enable-clipboard t)
      (setq xclip-mode t)
      (setq xclip-method (quote wl-copy))

      (setq org-agenda-files '("~/Documents/org/Todo.org"))
      (setq org-latex-compiler "xelatex")
      (find-file "~/Documents/org/Todo.org")
    '';
  };
  services.emacs = {
    enable = true;
    startWithUserSession = true;
  };
}
