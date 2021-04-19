{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.hardware.recoveryUsb;
in
{
  options.magic_rb.hardware.recoveryUsb = mkEnableOption "Enable recovery USB.";

  config = mkIf cfg {
    boot.supportedFilesystems = [ "zfs" ];
  };
}
