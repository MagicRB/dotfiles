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
  defaultBtrfsOpts = [
    "noatime"
    "space_cache"
  ];
  cfg = config.magic_rb.hardware.heater;
  kernel = pkgs.linuxKernel.packages.linux_xanmod_latest;
in {
  options.magic_rb.hardware.heater = mkEnableOption "Enable heater.";

  config = mkIf cfg {
    boot = {
      supportedFilesystems = ["zfs"];
      zfs.enableUnstable = true;

      initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usbhid"
        "usb_storage"
        "sd_mod"
        "nvme"
      ];
      initrd.kernelModules = ["dm-snapshot"];
      kernelModules = ["i2c-dev" "kvm-amd"];
      kernelParams = [
        "zfs.zfs_arc_max=8589934592"
        "nvidia.NVreg_EnablePCIeGen3=1"
        "nvidia.NVreg_UsePageAttributeTable=1"
        "nvidia-drm.modeset=1"
      ];
      extraModulePackages = [];
      kernelPackages = kernel;
    };

    hardware.nvidia.package = kernel.nvidia_x11_beta;
    hardware.firmware = singleton pkgs.firmwareLinuxNonfree;

    powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

    fileSystems = {
      "/" = {
        device = "heater-zpool/local/root";
        fsType = "zfs";
      };

      "/nix" = {
        device = "heater-zpool/local/nix";
        fsType = "zfs";
      };

      "/home" = {
        device = "heater-zpool/safe/home";
        fsType = "zfs";
      };

      "/var/lib/nomad" = {
        device = "heater-zpool/persist/nomad";
        fsType = "zfs";
      };

      "/var/lib/syncthing" = {
        device = "heater-zpool/persist/syncthing";
        fsType = "zfs";
      };

      "/etc/vault-agent" =
        mkIf config.services.vault-agent.enable
        {
          device = "heater-zpool/persist/vault-agent";
          fsType = "zfs";
        };

      "/boot" = {
        device = "/dev/disk/by-uuid/5e590840-9e62-4231-8ac5-e6a27325254d";
        fsType = "ext4";
      };

      "/boot/EFI" = {
        device = "/dev/disk/by-uuid/D381-9D12";
        fsType = "vfat";
      };

      "/mnt/cartman" = {
        device = "10.64.2.1:/mnt/cartman";
        fsType = "nfs";
        options = [ "_netdev" "hard" "async" ];
      };

      "/mnt/kyle" = {
        device = "10.64.2.1:/mnt/kyle";
        fsType = "nfs";
        options = [ "_netdev" "hard" "async" ];

      };

      "/mnt/stan" = {
        device = "10.64.2.1:/mnt/stan";
        fsType = "nfs";
        options = [ "_netdev" "hard" "async" ];
      };
    };

    swapDevices = [];
  };
}
