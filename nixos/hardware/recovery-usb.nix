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
  cfg = config.magic_rb.hardware.recoveryUsb;
in {
  options.magic_rb.hardware.recoveryUsb = mkEnableOption "Enable recovery USB.";

  config = mkIf cfg {
    # boot.supportedFilesystems = [ "zfs" ];
  };
}
