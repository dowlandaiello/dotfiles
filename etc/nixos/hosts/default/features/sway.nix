{ config, ... }:

{
  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      modifier = "Mod4";
      terminal = "alacritty";
      window.border = 1;
      colors.focused = {
        border = config.colorScheme.palette.base01;
        background = config.colorScheme.palette.base01;
        text = config.colorScheme.palette.base04;
        childBorder = config.colorScheme.palette.base01;
        indicator = config.colorScheme.palette.base01;
      };
    };
  };
}
