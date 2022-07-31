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
        device = "storfa/ds1/cartman";
        fsType = "zfs";
      };

      "/mnt/kyle" = {
        device = "storfa/ds1/kyle";
        fsType = "zfs";
      };

      "/mnt/stan" = {
        device = "storfa/ds1/stan";
        fsType = "zfs";
      };

      "/mnt/net/Magic_RB" = {
        fsType = "nfs";
        device = "${secret.network.ips.blowhole.ip}:/var/nfs/Magic_RB";
        options = [
          "hard"
          "async"
          "tcp"
          "fsc"
        ];
      };

      "/var/cache/fscache" = {
        device = "heater-zpool/persist/cachefilesd";
        fsType = "zfs";
      };
    };

    systemd.services.mnt-kyle-zfs-relmount = {
      requires = ["mnt-kyle.mount"];
      after = ["mnt-kyle.mount"];

      path = with pkgs; [zfs utillinux];

      serviceConfig = {
        RemainAfterExit = true;
        Type = "oneshot";
        ExecStart = "${pkgs.zfs-relmount}/bin/zfs-relmount storfa/ds1/kyle /mnt/kyle";
      };
    };

    systemd.services.mnt-cartman-zfs-relmount = {
      requires = ["mnt-cartman.mount"];
      after = ["mnt-cartman.mount"];

      path = with pkgs; [zfs utillinux];

      serviceConfig = {
        RemainAfterExit = true;
        Type = "oneshot";
        ExecStart = "${pkgs.zfs-relmount}/bin/zfs-relmount storfa/ds1/cartman /mnt/cartman";
      };
    };

    systemd.services.mnt-stan-zfs-relmount = {
      requires = ["mnt-stan.mount"];
      after = ["mnt-stan.mount"];

      path = with pkgs; [zfs utillinux];

      serviceConfig = {
        RemainAfterExit = true;
        Type = "oneshot";
        ExecStart = "${pkgs.zfs-relmount}/bin/zfs-relmount storfa/ds1/stan /mnt/stan";
      };
    };

    services.cachefilesd = {
      enable = true;
      cacheDir = "/var/cache/fscache";
    };

    swapDevices = [];
  };
}
