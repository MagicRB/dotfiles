{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, ... }:

{
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
    ];
    kernelPackages = nixpkgs-unstable.linuxPackages_latest;
  };

  hardware.enableRedistributableFirmware = true;

  powerManagement.cpuFreqGovernor = nixpkgs.lib.mkDefault "schedutil";

  fileSystems = {
    "/" =
      { device = "omen-zpool/root";
        fsType = "zfs";
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


    "/boot/efi" =
      { device = "/dev/disk/by-partlabel/efi-WL157385";
        fsType = "vfat";
      };
  };

  swapDevices = [
    { device = "/dev/disk/by-partlabel/swap-WL157385"; }
  ];
}
  
