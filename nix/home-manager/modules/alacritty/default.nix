{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.programs.alacritty;
in
{
  options.magic_rb.programs.alacritty = {
    enable = mkEnableOption "Enable the alacritty terminal emulator";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      alacritty
    ];

    home.file = {
      ".config/alacritty/alacritty.yaml".source = ./alacritty.yaml;
    };
  };
}
