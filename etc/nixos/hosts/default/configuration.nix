{ config, lib, pkgs, inputs, ... }:

let system = "x86_64-linux";
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.default
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # ZFS config
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.requestEncryptionCredentials = true;

  security.polkit.enable = true;
  services.upower.enable = true;
  services.fwupd.enable = true;
  services.fprintd.enable = false;

  services.zfs.autoScrub.enable = true;

  networking.hostName = "dggLnixsigma"; # Define your hostname.
  networking.hostId = "e5bf82bb";
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable =
    true; # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/NewYork";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "iosevka";
    useXkbConfig = true; # use xkb.options in tty.
  };

  fonts.packages = with pkgs; [
    font-awesome
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts.githubRelease
    dina-font
    proggyfonts
    roboto
    iosevka
    nerdfonts
  ];

  # Zsh
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" ];
    };
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  nixpkgs.config.pulseaudio = true;

  # Configure keymap in X11
  services.xserver = {
    enable = true;

    xkb = {
      layout = "us";
      options = "ctrl:swapcaps";
    };

    displayManager.startx.enable = true;
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd startx";
      };
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.pipewire.enable = false;

  # OpenGL
  hardware.graphics.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dowlandaiello = {
    isNormalUser = true;
    initialPassword = "password";
    extraGroups = [ "wheel" "docker" "podman" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.nushell;
    packages = with pkgs; [ ];
  };

  home-manager = {
    users = { "dowlandaiello" = import ./home.nix; };

    extraSpecialArgs = { inherit inputs; };
  };

  xdg.portal.config = { common = { default = [ "gtk" ]; }; };

  environment.etc."X11/xinit/xinitrc" =
    let mywm = inputs.mywm.packages.${system}.mywm;
    in {
      text = ''
        #!/bin/sh
        ${pkgs.feh}/bin/feh --bg-scale ~/Pictures/wallpapers/wa.jpg
        RUST_LOG=info ${mywm}/bin/mywm >>/home/dowlandaiello/.config/mywm/logfile 2>&1 &
        ${pkgs.polybar}/bin/polybar main >>/home/dowlandaiello/.config/polybar/logfile 2>&1
      '';
    };
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    emacs
    home-manager
    git
    firefox
    killall
    neofetch
    ripgrep
    gnumake
    nixfmt-classic
    lsof
    zip
    unzip
    openssl
    pkg-config
    libiconv
    dconf
    xorg.xmodmap
    xorg.xwd
    pulseaudioFull
    (polybar.override { mpdSupport = true; })
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;
  networking.nat.enable = true;
  networking.nat.internalInterfaces = [ "ve+" ];
  networking.nat.externalInterface = "enp0s31f6";

  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "23.11"; # Did you read the comment?
}

