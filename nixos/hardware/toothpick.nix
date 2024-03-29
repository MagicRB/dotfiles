# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  pkgs,
  config,
  lib,
  modulesPath,
  ...
} @ all:
with lib; let
  cfg = config.magic_rb.hardware.toothpick;
  qemu-guest = import (modulesPath + "/profiles/qemu-guest.nix") all;
in {
  options.magic_rb.hardware.toothpick = mkEnableOption "Enable omen.";

  config = mkIf cfg (mkMerge
    [
      {
        boot.loader.grub = {
          device = "/dev/vda";
          enable = true;
          version = 2;
        };
        boot.initrd.kernelModules = ["nvme"];
        fileSystems."/" = {
          device = "/dev/vda1";
          fsType = "ext4";
        };
      }
      qemu-guest
    ]);
}
