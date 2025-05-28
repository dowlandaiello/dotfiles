{ config, inputs, ... }:

let
  layoutConfig = import ./layout.nix;
  hexToRGBString = inputs.nix-colors.lib.conversions.hexToRGBString;
in
{
  programs.waybar = {
    enable = true;
    settings = [{
      layer = "top";
      position = "top";

      modules-left = [ "hyprland/workspaces" "hyprland/window" ];
      modules-right = [ "pulseaudio" "cpu" "memory" "battery" "clock" "tray" ];
      margin = "${toString layoutConfig.gapsOut} ${toString layoutConfig.gapsOut} 0 ${toString layoutConfig.gapsOut}";

      "clock" = {
        format = " {:L%I:%M %p}";
      	tooltip = true;
	tooltip-format = "<big>{:%A, %d.%B %Y }</big><tt><small>{calendar}</small></tt>";
      };

      "hyprland/workspaces" = {
        format = "{name}";
        format-icons = {
          default = " ";
          active = " ";
          urgent = " ";
        };
      };

      "hyprland/window" = {
        max-length = 25;
        separate-outputs = false;
      };

      "memory" = {
      	interval = 5;
      	format = " {}%";
        tooltip = true;
      };

      "cpu" = {
      	interval = 5;
      	format = " {usage:2}%";
        tooltip = true;
      };

      "disk" = {
        format = " {free}";
        tooltip = true;
      };

      "network" = {
        format-icons = ["󰤯" "󰤟" "󰤢" "󰤥" "󰤨"];
        format-ethernet = " {bandwidthDownOctets}";
        format-wifi = "{icon} {signalStrength}%";
        format-disconnected = "󰤮";
        tooltip = false;
      };

      "tray" = {
        spacing = 12;
      };

      "pulseaudio" = {
        format = "{icon} {volume}% {format_source}";
        format-bluetooth = "{volume}% {icon} {format_source}";
        format-bluetooth-muted = " {icon} {format_source}";
        format-muted = " {format_source}";
        format-source = " {volume}%";
        format-source-muted = "";
        format-icons = {
          headphone = "";
          hands-free = "";
          headset = "";
          phone = "";
          portable = "";
          car = "";
          default = ["" "" ""];
        };
        on-click = "sleep 0.1 && pavucontrol";
      };
    }];
    style = ''
      window#waybar {
        background-color: rgba(${hexToRGBString "," config.colorScheme.palette.base00},0.7);
        border: 0.5px solid #${config.colorScheme.palette.base0A};
        border-radius: 5px;
        color: white;
      }

      window>box {
        padding: 5px;
      }

      #workspaces, #window, #pulseaudio, #cpu, #memory, #battery, #clock, #tray {
        margin: 5px;
      }

      #workspaces button.active {
        background-color: #${config.colorScheme.palette.base0A};
        color: #${config.colorScheme.palette.base01};
      }
    '';
  };
}
