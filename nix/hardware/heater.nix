{ config, pkgs, lib, ... }:
with lib;
let
  defaultBtrfsOpts = [
    "noatime"
    "space_cache"
  ];
  cfg = config.magic_rb.hardware.heater;
in {
  options.magic_rb.hardware.heater = mkEnableOption "Enable heater.";

  config = mkIf cfg {
    boot = {
      supportedFilesystems = [ "zfs" ];
      zfs.enableUnstable = true;

      initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usbhid"
        "usb_storage"
        "sd_mod"
        "nvme"
      ];
      initrd.kernelModules = [ "dm-snapshot" ];
      kernelModules = [ "kvm-amd" ];
      extraModulePackages = [ ];
      kernelPackages = pkgs.linuxPackages_latest;
    };

    powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

    fileSystems = {
      "/" =
        {
          device = "heater-zpool/local/root";
          fsType = "zfs";
        };
      
      "/nix" =
        {
          device = "heater-zpool/local/nix";
          fsType = "zfs"; 
        };

      "/home" =
        {
          device = "heater-zpool/safe/home";
          fsType = "zfs";
        };
	    
      "/var/lib/nomad" =
        {
          device = "heater-zpool/persist/nomad";
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

      "/etc/vault-agent" = mkIf config.services.vault-agent.enable
        {
          device = "heater-zpool/persist/vault-agent";
          fsType = "zfs";
        };

      "/boot" =
        {
          device = "/dev/disk/by-uuid/5e590840-9e62-4231-8ac5-e6a27325254d";
          fsType = "ext4";
        };

      "/boot/EFI" =
        {
          device = "/dev/disk/by-uuid/D381-9D12";
          fsType = "vfat";
        };

      "/mnt/data1" =
        {
          device = "/dev/disk/by-uuid/0ab799ad-c5cf-4a5d-b5cc-7891115eb5e4";
          fsType = "btrfs";
          options = [
            "subvol=/"
          ] ++ defaultBtrfsOpts;
        };

      "/mnt/data2" =
        {
          device = "/dev/mapper/VG_HDD2-Data0";
          fsType = "ext4";
        };

      "/mnt/data3" =
        {
          device = "/dev/mapper/VG_0-LV_Data0";
          fsType = "ext4";
        };

      "/mnt/net/Magic_RB" =
        {
          fsType = "nfs";
          device = "blowhole.in.redalder.org:/Magic_RB";
        };
    };

    #  fileSystems."/mnt/win10" =
    #    {
    #      device = "/dev/disk/by-uuid/44394608-0b1b-46b2-9f2e-4e9afde83f5b";
    #      fsType = "ntfs-3g";
    #      options = [
    #        "uid=${builtins.toString config.users.users.main.uid}"
    #        "gid=${builtins.toString config.users.groups.main.gid}"
    #        "umask=002"
    #      ];
    #    };

    swapDevices = [ ];
  };
}
