{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.grub;
in
{
  options.magic_rb.grub = with lib; {
    enable = mkEnableOption "GRUB preconfigured for my setup style";
    efi = mkOption {
      type = types.submodule {
        options.enable = mkEnableOption "EFI support";
      };
      default = {};
    };
      
    devices = mkOption {
      description = "GRUB devices for legacy";
      default = [];
      type = types.list;
    };
  };

  config = {
    boot.loader = mkIf cfg.enable {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = cfg.efi.enable;
      efi.efiSysMountPoint = "/boot/EFI";

      grub = {
        enable = true;
        efiSupport = cfg.efi.enable;
        version = 2;
        devices = if cfg.efi.enable then [ "nodev" ] else cfg.devices;
      };
    };
  };
}
