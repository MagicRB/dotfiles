{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections.cmdline;
in
{
  options.magic_rb.packageCollections.cmdline = {
    enable = mkEnableOption
      ''
        A package collection containing command line programs, specifically zip, unzip, unrar (unfree), git, and htop.
      '';
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      zip
      unzip
      unrar
      git
      htop
    ];
  };
}
