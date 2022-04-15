{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.programs.xmonad;
in
{
  options.magic_rb.programs.xmonad = {
    enable = mkEnableOption "Enable xmonad config";

    enableDunst = mkOption {
      description = "Enable dunst";
      type = types.bool;
      default = true;
    };

    enablePicom = mkOption {
      description = "Enable picom";
      type = types.bool;
      default = true;
    };

    enableKeynav = mkOption {
      description = "Enable keynav";
      type = types.bool;
      default = true;
    };

    picomExperimentalBackends = mkOption {
      description = "Enable experimental backends in picom";
      type = types.bool;
      default = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      powerline-fonts
      font-awesome
      dejavu_fonts
    ];

    home.file.".keynavrc".source = ./keynavrc;

    home.file.".xmonad/xmonad.hs".source = pkgs.writeSubstitutedFile {
      name = "xmonad.hs";
      file = ./xmonad.hs;
      substitutes = {
        "xmobar" = "${pkgs.xmobar}/bin/xmobar";
        "xmobarConfig" = ./xmobarrc;
        "screenshot" = "${pkgs.magic_rb.screenshot}/bin/screenshot";
        "dmenu_run" = "${pkgs.dmenu}/bin/dmenu_run";

        "dunst" = "${pkgs.dunst}/bin/dunst";
        "enableDunst" = if cfg.enableDunst then "True" else "False";
        "dunstConfig" = ./dunstrc;

        "picom" = "${pkgs.picom}/bin/picom";
        "enablePicom" = if cfg.enablePicom then "True" else "False";
        "picomConfig" = ./picom.conf;
        "experimentalBackends" = if cfg.picomExperimentalBackends then "--experimental-backends" else "";

        "keynav" = "${pkgs.keynav}/bin/keynav";
        "enableKeynav" = if cfg.enableKeynav then "True" else "False";
      };
    };
  };
}
