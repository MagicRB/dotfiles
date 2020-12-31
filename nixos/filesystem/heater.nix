{ config, pkgs, ... }:

let
  defaultBtrfsOpts = [
    "noatime"
    "space_cache"
  ];
in {
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/7831f8ee-0ad6-49c2-8288-6f5e61ae0a08";
      fsType = "btrfs";
      options = [ 
        "subvol=/nix"
      ] ++ defaultBtrfsOpts;
    };

  fileSystems."/btrfs" =
    {
      device = "/dev/disk/by-uuid/7831f8ee-0ad6-49c2-8288-6f5e61ae0a08";
      fsType = "btrfs";
      options = [ 
        "subvol=/"
      ] ++ defaultBtrfsOpts;
    };

  fileSystems."/boot/EFI" =
    {
      device = "/dev/disk/by-uuid/DBE9-10B4";
      fsType = "vfat";
    };

  fileSystems."/mnt/data1" =
    {
      device = "/dev/disk/by-uuid/0ab799ad-c5cf-4a5d-b5cc-7891115eb5e4";
      fsType = "btrfs";
      options = [
        "subvol=/"
      ] ++ defaultBtrfsOpts;
    };

  fileSystems."/mnt/data2" =
    {
      device = "/dev/mapper/VG_HDD2-Data0";
      fsType = "ext4";
    };

  fileSystems."/mnt/data3" =
    {
      device = "/dev/mapper/VG_0-LV_Data0";
      fsType = "ext4";
    };

  swapDevices = [ ];
}
