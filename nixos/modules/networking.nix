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
  cfg = config.magic_rb.networking;
in {
  options.magic_rb.networking = {
    networkManager = mkEnableOption "Whether to enable Network Manager, usefull on laptops.";
    bluetooth = mkEnableOption "Whether to enable bluetooth.";
  };

  config = {
    networking.networkmanager.enable = mkIf cfg.networkManager true;
    hardware.bluetooth.enable = mkIf cfg.bluetooth true;
  };
}
