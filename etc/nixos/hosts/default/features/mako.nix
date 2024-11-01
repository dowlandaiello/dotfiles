{ config, ... }:

{
  services.mako = {
    enable = true;
    backgroundColor = "#${config.colorScheme.palette.base01}";
    borderColor="#${config.colorScheme.palette.base0A}";
    borderRadius = 5;
    borderSize = 1;
    textColor = "#${config.colorScheme.palette.base04}";
    layer = "overlay";
  };
}