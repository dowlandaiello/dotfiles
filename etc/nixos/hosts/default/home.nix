{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    inputs.nix-colors.homeManagerModules.default
    ./features/xdg.nix
    ./features/mako.nix
    ./features/alacritty.nix
    ./features/gtk.nix
    ./features/qt.nix
    ./features/zsh.nix
    ./features/emacs.nix
    ./features/git.nix
    ./features/direnv.nix
    ./features/nushell.nix
    ./features/polybar.nix
  ];

  colorScheme = inputs.nix-colors.colorSchemes.default-light;

  dconf = {
    enable = true;

    settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-light";
      };
    };
  };

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "slack" "zoom" "obsidian" "vscode-extension-ms-vscode-cpptools" ];

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "dowlandaiello";
  home.homeDirectory = "/home/dowlandaiello";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = let
    my_dmenu = pkgs.writeShellScriptBin "mydmenu_run" ''
      #!/bin/sh
      dmenu_run  -nb "#${config.colorScheme.palette.base00}" -nf "#${config.colorScheme.palette.base07}" -sf "#${config.colorScheme.palette.base00}" -sb "#${config.colorScheme.palette.base07}"
    '';
  in with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    dmenu
    my_dmenu
    feh
    gruvbox-gtk-theme
    zsh-syntax-highlighting
    python311Packages.python-lsp-server
    vesktop
    xclip
    slack
    zoom-us
    black
    python3
    go
    libgcc
    gcc
    cargo
    rustc
    rust-analyzer
    rustfmt
    lean4
    pavucontrol
    docker-compose
    tor-browser
    protobuf
    nodePackages.typescript-language-server
    typescript
    kdePackages.kleopatra
    signal-desktop
    obsidian
    chromium
    flameshot
    vale
    inputs.proselint.packages.${system}.default
    lldb
    gdb
    llvm
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
    # ".vale.ini".text = ''
    #   StylesPath = styles
    #
    #   Vocab = Blog
    #
    #   [*.org]
    #   BasedOnStyles = Microsoft
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/dowlandaiello/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "emacs";
    SHELL = "zsh";
    GOPATH = "/home/dowlandaiello/go";
    GOBIN = "/home/dowlandaiello/go/bin";
    PATH = "$PATH:/home/dowlandaiello/go/bin";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
