{ config, lib, pkgs, ... }:
with lib;
let
  nglib = config.magic_rb.pins.nixng.lib pkgs.stdenv.system;
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

    home.file.".xmonad/xmonad.hs".source = nglib.writeSubstitutedFile {
      name = "xmonad.hs";
      file = ./xmonad.hs;
      substitutes = {
        "xmobar" = "${pkgs.xmobar}/bin/xmobar";
        "xmobarConfig" = ./xmobarrc;
        "screenshot" = "${pkgs.magic_rb.screenshot}/bin/screenshot";
        "dmenu_run" = "${pkgs.dmenu}/bin/dmenu_run";

        "enableDunst" = if cfg.enableDunst then "True" else "False";
        "dunstConfig" = ./dunstrc;

        "enablePicom" = if cfg.enablePicom then "True" else "False";
        "picomConfig" = ./picom.conf;
        "experimentalBackends" = if cfg.picomExperimentalBackends then "--experimental-backends" else "";
      };
    };
  };
}
