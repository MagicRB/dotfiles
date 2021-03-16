{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.erase-my-darlings;
in
# Send love to https://grahamc.com/blog/erase-your-darlings
{
  options = {
    magic_rb.erase-my-darlings = {
      enable = mkEnableOption "Erase the root filesystem using ZFS snapshots on boot.";

      snapshot = mkOption {
        type = types.str;
        description = "Which snapshot to rollback to, also specifies which dataset to rollback.";
      };
    };
  };

  config = mkIf cfg.enable {
    boot.initrd.postDeviceCommands = lib.mkAfter ''
      zfs rollback -r ${cfg.snapshot}
    '';
  };
}
