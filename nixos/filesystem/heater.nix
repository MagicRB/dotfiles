{ config, pkgs, ... }:

{
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/7831f8ee-0ad6-49c2-8288-6f5e61ae0a08";
      fsType = "btrfs";
      options = [ 
        "subvol=/nix"
      ];
    };

  fileSystems."/boot/EFI" =
    { device = "/dev/disk/by-uuid/DBE9-10B4";
      fsType = "vfat";
    };

  swapDevices = [ ];
}
