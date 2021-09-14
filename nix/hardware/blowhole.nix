{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.magic_rb.hardware.blowhole;
in {
  options.magic_rb.hardware.blowhole = mkEnableOption "Enable blowhole.";

  config = mkIf cfg {
    boot = {
      kernelPackages = pkgs.linuxPackages_latest;
    };

    fileSystems = {
      "/" =
        {
          device = "asd";
          fsType = "ext4";
        };
    };
  };
}
