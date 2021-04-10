{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections.wine;
  inherit (config.magic_rb.pkgs) nixpkgs-unstable;
in
{
  options.magic_rb.packageCollections.wine = {
    enable = mkEnableOption "Enable wine package collection, contains wine-staging and winetricks";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      winetricks
    ] ++ (with nixpkgs-unstable; [
      wineWowPackages.staging
    ]);
  };
}
