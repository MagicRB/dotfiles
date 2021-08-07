{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.magic_rb.hardware.gooseberry;
in {
  options.magic_rb.hardware.gooseberry = mkEnableOption "Enable gooseberry.";

  config = mkIf cfg {
    boot = {
      initrd.availableKernelModules = [
        "sd_mod"
      ];

      kernelPackages = pkgs.linuxPackages_latest;
    };

    hardware.firmware = with pkgs;
      [ raspberrypiWirelessFirmware
      ];

    fileSystems = {
      "/" =
        {
          device = "/dev/disk/by-uuid/e8d0f836-0851-45b2-801f-8cb2bfc016ac";
          fsType = "btrfs";
          options = [
            "space_cache"
            "noatime"
            "subvol=/local/root"
          ];
        };

      "/nix" =
        {
          device = "/dev/disk/by-uuid/e8d0f836-0851-45b2-801f-8cb2bfc016ac";
          fsType = "btrfs";
          options = [
            "space_cache"
            "noatime"
            "subvol=/local/nix"
          ];
        };


      "/home" =
        {
          device = "/dev/disk/by-uuid/e8d0f836-0851-45b2-801f-8cb2bfc016ac";
          fsType = "btrfs";
          options = [
            "space_cache"
            "noatime"
            "subvol=/safe/home"
          ];
        };

      "/var/lib/klipper/sdcard" =
        {
          device = "/dev/disk/by-uuid/e8d0f836-0851-45b2-801f-8cb2bfc016ac";
          fsType = "btrfs";
          options = [
            "space_cache"
            "noatime"
            "subvol=/safe/klipper-sdcard"
          ];
        };

      "/btrfs" =
        {
          device = "/dev/disk/by-uuid/e8d0f836-0851-45b2-801f-8cb2bfc016ac";
          fsType = "btrfs";
          options = [
            "space_cache"
            "noatime"
            "subvol=/"
          ];
        };

      "/boot" =
        {
          device = "/dev/disk/by-uuid/f358d9fd-1ae2-4a60-89f4-1da2146ef92d";
          fsType = "ext4";
        };

      "/boot/EFI" =
        {
          device = "/dev/disk/by-uuid/61DC-10CA";
          fsType = "vfat";
        };
    };
  };
}
