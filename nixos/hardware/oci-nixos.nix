# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  hostName,
  rootUUID,
  efiUUID,
  swapUUID,
}: {
  config,
  pkgs,
  lib,
  modulesPath,
  ...
} @ all:
with lib; let
  cfg = config.magic_rb.hardware."${hostName}";
  qemu-guest = import (modulesPath + "/profiles/qemu-guest.nix") all;
in {
  options.magic_rb.hardware."${hostName}" = mkEnableOption "";

  config = mkIf cfg (mkMerge
    [
      {
        boot = {
          kernelPackages = pkgs.linuxPackages_latest;
          loader.grub.extraConfig = ''
            serial --unit=0 --speed=115200 --word=8 --parity=no --stop=1
            terminal_input --append serial
            terminal_output --append serial
          '';

          initrd.kernelModules = [
            "nvme"
          ];
        };

        swapDevices = [
          {
            device = "/dev/disk/by-uuid/${swapUUID}";
          }
        ];

        fileSystems = {
          "/" = {
            device = "/dev/disk/by-uuid/${rootUUID}";
            fsType = "xfs";
          };

          "/boot/EFI" = {
            device = "/dev/disk/by-uuid/${efiUUID}";
            fsType = "vfat";
          };
        };
      }
      qemu-guest
    ]);
}
