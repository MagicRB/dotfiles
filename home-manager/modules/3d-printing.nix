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
  };
}
