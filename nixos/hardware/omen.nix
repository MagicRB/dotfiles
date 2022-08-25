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
  cfg = config.magic_rb.hardware.omen;
in {
  options.magic_rb.hardware.omen = mkEnableOption "Enable omen.";

  config = mkIf cfg {
    boot = {
      initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usb_storage"
        "sr_mod"
        "rtsx_pci_sdmmc"
        "nvme"
      ];
      initrd.kernelModules = [];
      kernelModules = ["kvm-intel"];
      extraModulePackages = [];
      supportedFilesystems = ["zfs"];
      kernelParams = [
        "zfs.zfs_arc_max=8589934592"
        "intel_pstate=active"
        "nvidia.NVreg_EnablePCIeGen3=1"
        "nvidia.NVreg_UsePageAttributeTable=1"
        "nvidia-drm.modeset=1"
      ];
      kernelPackages = pkgs.zfsUnstable.latestCompatibleLinuxPackages;
      zfs.enableUnstable = true;
    };

    hardware.enableRedistributableFirmware = true;
    hardware.nvidia.package = pkgs.zfsUnstable.latestCompatibleLinuxPackages.nvidia_x11_beta;

    powerManagement.cpuFreqGovernor = pkgs.lib.mkDefault "schedutil";

    fileSystems = {
      "/" = {
        device = "omen-ssd/local/root";
        fsType = "zfs";
      };

      "/var/lib/secrets" = {
        device = "omen-ssd/local/secrets";
        fsType = "zfs";
      };

      "/home" = {
        device = "omen-ssd/safe/home";
        fsType = "zfs";
      };

      "/nix" = {
        device = "omen-ssd/local/nix";
        fsType = "zfs";
      };

      "/boot" = {
        device = "/dev/disk/by-uuid/078c1885-5e0c-4bb8-bec3-5bf40785f5cd";
        fsType = "ext4";
      };

      "/mnt/net/Magic_RB" = {
        fsType = "nfs";
        device = "10.64.0.2:/Magic_RB";
        options = [
          "noauto"
          "x-systemd.automount"
          "x-systemd.device-timeout=10"
          "timeo=14"
          "hard"
          "intr"
          "noatime"
          "x-systemd.after=wireguard-wg0.service"
        ];
      };

      "/boot/EFI" = {
        device = "/dev/disk/by-uuid/6F1E-8B1B";
        fsType = "vfat";
      };
    };

    swapDevices = [
      # { device = "/dev/disk/by-partlabel/swap-WL157385"; }
    ];
  };
}
