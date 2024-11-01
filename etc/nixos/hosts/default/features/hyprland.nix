{ config, pkgs, inputs, ... }:

let
  startupScript = pkgs.pkgs.writeShellScriptBin "start" ''
    swww-daemon &

    waybar &

    sleep 1

    swww img ~/Images/wallpapers/totoro-in-the-flower-field-moewalls-com.gif
  '';
in
let
  newTermScript = pkgs.pkgs.writeShellScriptBin "start" ''
    pid="$(hyprctl activewindow | grep pid | grep -o '[0-9]*')"
    ppid="$(ps -o pid= --ppid "$pid" | xargs)"
    alacritty --working-directory "$(readlink "/proc/$ppid/cwd")"
'';
in
let
  layoutConfig = import ./layout.nix;
in
{
  wayland.windowManager.hyprland = {
    enable = true;

    settings = {
      "$mod" = "SUPER";

      general = {
        "$terminal" = "${pkgs.alacritty}/bin/alacritty";
        monitor = "eDP-1,2560x1600,0x0,1";
        "col.active_border" = "0x${config.colorScheme.palette.base0A}";
        border_size = 2;
        gaps_out = layoutConfig.gapsOut;
      };

      exec-once = "${startupScript}/bin/start";

      bind = [
        "$mod, RETURN, exec, ${pkgs.alacritty}/bin/alacritty"
        "$mod SHIFT, RETURN, exec, ${newTermScript}/bin/start"
        "CTRL SHIFT, SPACE, exec, anyrun"
        ", Print, exec, grimshot --notify save screen"
        "$mod, L, resizeactive, 10 0"
        "$mod, H, resizeactive, -10 0"
        "$mod, K, resizeactive, 0 -10"
        "$mod, J, resizeactive, 0 10"
        "$mod, N, cyclenext, "
        "$mod, P, cyclenext, prev"
        "$mod, F, fullscreen, "
        "$mod SHIFT, C, killactive, "
        "$mod SHIFT, N, swapwindow, l"
        "$mod SHIFT, P, swapwindow, r"
      ] ++ (
        builtins.concatLists (builtins.genList (
          x: let
            ws = let
              c = (x + 1) / 5;
            in
              builtins.toString (x + 1 - (c * 5));
          in [
            "$mod, ${ws}, workspace, ${toString (x + 1)}"
            "$mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
          ]
        ) 5
      ));

      input = {
        kb_options = "ctrl:nocaps";
      };

      decoration = {
        rounding = 5;

        blur = {
          enabled = true;
          size = 5;
          passes = 1;
          new_optimizations = true;
        };

        active_opacity = 0.85;
        inactive_opacity = 0.6;

        "col.shadow" = "0x${config.colorScheme.palette.base06}66";
      };

      misc = {
        disable_splash_rendering = true;
        force_default_wallpaper = 0;
      };
    };
  };
}
