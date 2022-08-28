# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.magic_rb.packageCollections."3dPrinting";
in {
  options.magic_rb.packageCollections."3dPrinting" = {
    enable =
      mkEnableOption
      ''
        Enable 3D printing package collection, contains Prusa Slicer,
        Cura, OpenSCAD, and inkscape."
      '';
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      openscad
      cura
      inkscape
#       prusa-slicer
      freecad
    ];

    home.file.".local/share/OpenSCAD/libraries/BOSL2".source =
      pkgs.fetchFromGitHub {
        owner = "revarbat";
        repo = "BOSL2";
        rev = "85e6dcd4835d019c8b582c03cf6e41bf83199cd1";
        sha256 = "sha256-fuOBp8231ODF4mRzilBfb/JePG+8ANdxkiHbA6a4wts=";
      };
  };
}
