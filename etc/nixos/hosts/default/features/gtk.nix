{ pkgs, ... }:

{
  gtk = {
    enable = true;
    theme = {
      name = "Gruvbox-Dark-Hard";
      package = pkgs.gruvbox-dark-gtk;
    };
  };
}
