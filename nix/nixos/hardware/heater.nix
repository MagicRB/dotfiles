{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, ... }:

let
  defaultBtrfsOpts = [
    "noatime"
    "space_cache"
  ];
in {
  boot = {
    initrd.availableKernelModules = [
      "xhci_pci"
      "ahci"
      "usbhid"
      "usb_storage"
      "sd_mod"
    ];
    initrd.kernelModules = [ "dm-snapshot" ];
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
    kernelPackages = nixpkgs-unstable.linuxPackages_latest;
  };

  powerManagement.cpuFreqGovernor = nixpkgs.lib.mkDefault "performance";

  fileSystems = {
    "/" =
      {
        device = "/dev/disk/by-uuid/7831f8ee-0ad6-49c2-8288-6f5e61ae0a08";
        fsType = "btrfs";
        options = [ 
          "subvol=/nix"
        ] ++ defaultBtrfsOpts;
      };

    "/btrfs" =
      {
        device = "/dev/disk/by-uuid/7831f8ee-0ad6-49c2-8288-6f5e61ae0a08";
        fsType = "btrfs";
        options = [ 
          "subvol=/"
        ] ++ defaultBtrfsOpts;
      };

    "/boot/EFI" =
      {
        device = "/dev/disk/by-uuid/DBE9-10B4";
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
}
