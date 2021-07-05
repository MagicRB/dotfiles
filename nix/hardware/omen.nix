{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.hardware.omen;
in
{
  options.magic_rb.hardware.omen = mkEnableOption "Enable omen.";

  config = mkIf cfg {
    boot = {
      initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usb_storage"
        "sr_mod"
        "rtsx_pci_sdmmc"
      ];
      initrd.kernelModules = [ ];
      kernelModules = [ "kvm-intel" ];
      extraModulePackages = [ ];
      supportedFilesystems = [ "zfs" ];
      kernelParams = [
        "zfs.zfs_arc_max=214748368"
        "intel_pstate=active"
      ];
      kernelPackages = pkgs.linuxPackages_latest;
      zfs.enableUnstable = true;
    };

    hardware.enableRedistributableFirmware = true;
    hardware.nvidia.package = pkgs.linuxPackages_latest.nvidia_x11_beta;

    powerManagement.cpuFreqGovernor = pkgs.lib.mkDefault "schedutil";

    fileSystems = {
      "/" =
        { device = "omen-zpool/root";
          fsType = "zfs";
        };

      "/var/lib/secrets" = mkIf config.services.vault-agent.enable
        {
          device = "tmpfs";
          fsType = "tmpfs";
          options = [
            "mode=0640"
            "uid=${toString config.users.users.vault-agent.uid}"
            "gid=${toString config.users.groups.root.gid}"
            "noexec"
            "rw"
            "size=64M"
          ];
        };

      "/home" =
        { device = "omen-zpool/root/home";
          fsType = "zfs";
        };

      "/nix" =
        { device = "omen-zpool/root/nix";
          fsType = "zfs";
        };

      "/boot" =
        { device = "/dev/disk/by-partlabel/boot-WL157385";
          fsType = "ext4";
        };

      "/mnt/net/Magic_RB" =
        {
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

      "/boot/efi" =
        { device = "/dev/disk/by-partlabel/efi-WL157385";
          fsType = "vfat";
        };
    };

    swapDevices = [
      { device = "/dev/disk/by-partlabel/swap-WL157385"; }
    ];
  };
}
  
