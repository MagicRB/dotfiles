{ config, pkgs, ... }:

{
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/bfdcd9bc-c5bb-424a-a110-20fab4b97f6d";
      fsType = "btrfs";
      options = [ "subvol=nixos" ];
    };

    "/btrfs" = {
      device = "/dev/disk/by-uuid/bfdcd9bc-c5bb-424a-a110-20fab4b97f6d";
      fsType = "btrfs";
      options = [ "subvol=/" ];
    };
  };

  swapDevices = [
   { device = "/dev/disk/by-uuid/9dca362e-43be-4f27-81bc-6bb7a244a7ae"; }
  ];
}
