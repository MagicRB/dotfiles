{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
{
  boot.loader = {
    systemd-boot.enable = false;
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/EFI";

    grub = {
      enable = true;
      efiSupport = true;
      version = 2;
      devices = [ "nodev" ];
    };
  };
}
