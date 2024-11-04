{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [
      nix-mode
      nix-sandbox
      vterm
      gruvbox-theme
      hardcore-mode
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
    ];
    extraConfig = ''
      ;; Remove GUI bloat
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (menu-bar-mode -1)

      (xclip-mode 1)
      (direnv-mode)

      ;; Alert errors
      (setq visible-bell t)
      (setq inhibit-startup-screen t)
      (setq inhibit-startup-message t)
      (setq initial-scratch-message nil)
      (setq inhibit-splash-screen t)

      ;; Visual preferences:
      ;; - dark theme
      ;; - bottom padding
      ;; - font
      (load-theme 'gruvbox-dark-hard :no-confirm)
      (set-fringe-mode 10)
      (set-face-attribute 'default nil :font "Iosevka")
      (set-face-attribute 'fixed-pitch nil :font "Iosevka")
      (set-face-attribute 'variable-pitch nil :font "Roboto" :weight 'regular)
      (window-divider-mode +1)
      (setq window-divider-default-right-width 2 window-divider-default-bottom-width 2)
      (doom-modeline-mode 1)

      (projectile-mode)
      (counsel-projectile-mode)
      (setq projectile-switch-project-action #'projectile-dired)
      (global-hardcore-mode)

      (setq vterm-max-scrollback 10000)
      (setq vterm-shell "nu")

      (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
      (add-hook 'web-mode-hook (lambda ()
                               (lsp-deferred)
                               (prettier-js-mode)))
      (add-hook 'typescript-mode-hook (lambda()
                                      (prettier-js-mode)
                                      (lsp-deferred)))
      (add-hook 'python-mode-hook 'lsp)
      (add-hook 'python-mode-hook 'python-black-on-save-mode)
      (add-hook 'rust-mode-hook 'lsp)
      (add-hook 'org-mode-hook (lambda ()
                               (configure-org)
                               (stylize-org)
                               (pad-org)))
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
      (setq flycheck-command-wrapper-function
              (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
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

      (setq auto-mode-alist
          (append
          '(("\\.tsx\\'" . web-mode)
          ("\\.rasi\\'" . prog-mode)) auto-mode-alist))

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
    '';
  };
}
