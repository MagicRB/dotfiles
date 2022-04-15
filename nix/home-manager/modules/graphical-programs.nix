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
      ungoogled-chromium
      (discord-canary.overrideAttrs ( old:
        { src = pkgs.fetchurl
          { url = "https://dl-canary.discordapp.net/apps/linux/0.0.134/discord-canary-0.0.134.tar.gz";
            sha256 = "sha256-HyJa6lGcKMPKWffO/pnNcn8fDTJj6O4J8Y5RA23a1kM=";
          };
        }
      ))
      element-desktop

      xournalpp
      pavucontrol
    ];
  };
}
