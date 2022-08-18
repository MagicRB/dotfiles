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
  cfg = config.magic_rb.packageCollections.graphical;
  inherit (config.magic_rb.pkgs) nixpkgs-master;
in {
  options.magic_rb.packageCollections.graphical = {
    enable =
      mkEnableOption
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
      webcord
      element-desktop

      xournalpp
      pavucontrol
    ];
  };
}
