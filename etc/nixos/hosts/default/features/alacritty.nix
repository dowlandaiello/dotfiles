{ config, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "IosevkaTerm Nerd Font";
          style = "Light";
        };
        bold = {
          family = "IosevkaTerm Nerd Font";
          style = "Medium";
        };
        italic = {
          family = "IosevkaTerm Nerd Font";
          style = "Light Italic";
        };
        size = 5;
      };
      colors = {
        bright = {
          black = "0x${config.colorScheme.palette.base00}";
          blue = "0x${config.colorScheme.palette.base0D}";
          cyan = "0x${config.colorScheme.palette.base0C}";
          green = "0x${config.colorScheme.palette.base0B}";
          magenta = "0x${config.colorScheme.palette.base0E}";
          red = "0x${config.colorScheme.palette.base08}";
          white = "0x${config.colorScheme.palette.base06}";
          yellow = "0x${config.colorScheme.palette.base09}";
        };
        cursor = {
          cursor = "0x${config.colorScheme.palette.base06}";
          text = "0x${config.colorScheme.palette.base06}";
        };
        normal = {
          black = "0x${config.colorScheme.palette.base00}";
          blue = "0x${config.colorScheme.palette.base0D}";
          cyan = "0x${config.colorScheme.palette.base0C}";
          green = "0x${config.colorScheme.palette.base0B}";
          magenta = "0x${config.colorScheme.palette.base0E}";
          red = "0x${config.colorScheme.palette.base08}";
          white = "0x${config.colorScheme.palette.base06}";
          yellow = "0x${config.colorScheme.palette.base0A}";
        };
        primary = {
          background = "0x${config.colorScheme.palette.base00}";
          foreground = "0x${config.colorScheme.palette.base06}";
        };
      };
      terminal.shell.program = "nu";
    };
  };
}
