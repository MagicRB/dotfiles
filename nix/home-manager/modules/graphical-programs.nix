{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections.graphical;
  inherit (config.magic_rb.pkgs) nixpkgs-master;
in
{
  options.magic_rb.packageCollections.graphical = {
    enable = mkEnableOption
      ''
        Enable graphical package collection, contains GIMP, Firefox, mpv, and Discord.
      '';
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gimp
      firefox
      mpv
      slack
      (discord-canary-system.override rec {
        isWayland = false;
        version = "0.0.131";
        src = fetchurl {
          url = "https://dl-canary.discordapp.net/apps/linux/${version}/discord-canary-${version}.tar.gz";
          sha256 = "sha256-ZYPdE02Jq79LmvXu7KIJFugJX++Nnj0Og/mBuaP/+SA";
        };
      })
      element-desktop
      nyxt
    ];
  };
}
