# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.magic_rb.packageCollections.wine;

  combineWines = wines:
    map (
      wine:
        pkgs.writeShellScriptBin wine.name
        ''
          ${wine}/bin/wine "$@"
        ''
    )
    wines;
in {
  options.magic_rb.packageCollections.wine = {
    enable = mkEnableOption "Enable wine package collection, contains wine-staging and winetricks";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs;
      [
        winetricks
      ]
      ++ combineWines (with pkgs; [
        wineWowPackages.staging
      ]);
  };
}
