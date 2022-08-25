# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  pkgs,
  lib,
  secret,
  ...
}:
with lib; let
  cfg = config.magic_rb.hardware.blowhole;
in {
  options.magic_rb.hardware.blowhole = mkEnableOption "Enable blowhole.";

  config = mkIf cfg {
    boot = {
      supportedFilesystems = ["zfs"];
      initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usbhid"
        "usb_storage"
        "sd_mod"
        "nvme"
      ];
      zfs.enableUnstable = true;
      kernelPackages = pkgs.zfsUnstable.latestCompatibleLinuxPackages;
    };

    fileSystems =
      {
        "/boot" = {
          device = "/dev/disk/by-uuid/738acc32-3e2e-4986-987c-40264153d5bf";
          fsType = "ext4";
        };
        "/" = {
          device = "blowhole-zpool/local/root";
          fsType = "zfs";
        };
        "/nix" = {
          device = "blowhole-zpool/local/nix";
          fsType = "zfs";
        };

        "/var/nfs" = {
          device = "/dev/disk/by-uuid/e06f6d2c-e434-4eec-b00d-b13c1ecc96f0";
          fsType = "btrfs";
          options = [
            "subvol=/nfs"
            "noatime"
          ];
        };

        "/old-root" = {
          device = "/dev/disk/by-uuid/e06f6d2c-e434-4eec-b00d-b13c1ecc96f0";
          fsType = "btrfs";
          options = [
            "subvol=/arch"
            "noatime"
          ];
        };
        "/var/lib/nomad" = {
          device = "/old-root/var/lib/nomad";
          options = singleton "bind";
        };
        "/var/secrets" = {
          device = "blowhole-zpool/persist/secrets";
          fsType = "zfs";
        };
        "/var/lib/consul" = {
          device = "/old-root/var/lib/consul";
          options = singleton "bind";
        };
        "/var/lib/vault" = {
          device = "/old-root/var/lib/vault";
          options = singleton "bind";
        };
      }
      // secret.mounts.blowhole;
  };
}
