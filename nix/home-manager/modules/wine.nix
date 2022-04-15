{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections.wine;
  inherit (config.magic_rb.pkgs) nixpkgs-unstable;

  combineWines = wines:
    map (wine: pkgs.writeShellScriptBin wine.name
      ''
        ${wine}/bin/wine "$@"
      ''
    ) wines;
in
{
  options.magic_rb.packageCollections.wine = {
    enable = mkEnableOption "Enable wine package collection, contains wine-staging and winetricks";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      winetricks
    ] ++ combineWines (with nixpkgs-unstable; [
      wineWowPackages.staging
    ]);
  };
}
