{ pkgs, inputs, ... }:

{
  programs.anyrun = {
    enable = true;
    config = {
      plugins = [
        inputs.anyrun.packages.${pkgs.system}.applications
      ];
      x = { fraction = 0.5; };
      y = { fraction = 0.3; };
      width = { fraction = 0.3; };
      hideIcons = true;
      layer = "overlay";
    };
    extraCss = ''
      .window {
        opacity: 0.85;
        border-radius: 10px;
        margin: 10px;
      }
    '';
  };
}