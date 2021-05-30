hostname:
{ config, pkgs, lib, modulesPath, ... }@all:
with lib;
let
  cfg = config.magic_rb.hardware."${hostname}";
  qemu-guest = import (modulesPath + "/profiles/qemu-guest.nix") all;
in
{
  options.magic_rb.hardware."${hostname}" = mkEnableOption "";

  config = mkIf cfg ({
    boot = {
      kernelPackages = pkgs.linuxPackages_latest;
      loader.grub.extraConfig = ''
        serial --unit=0 --speed=115200 --word=8 --parity=no --stop=1
        terminal_input --append serial
        terminal_output --append serial
      '';

      initrd.kernelModules = [
        "nvme"
      ];
    };

    swapDevices = [
      {
        device = "/dev/disk/by-uuid/d50e7ebf-8c62-4d2d-b19d-347378f7e5fe";
      } 
    ];
      
    fileSystems = {
      "/" =
        {
          device = "/dev/disk/by-uuid/79ba4403-7532-4e2c-ac5d-2910dce62009";
          fsType = "xfs";
        };

      "/boot/efi" =
        {
          device = "/dev/disk/by-uuid/4478-6009";
          fsType = "vfat";
        };
    };
  } // qemu-guest); 
}
