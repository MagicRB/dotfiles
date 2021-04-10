{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections.webdev;
in
{
  options.magic_rb.packageCollections.webdev = {
    enable = mkEnableOption
      ''
        Enable webdev package collection, contains yarn and wasm-pack.
      '';
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      yarn
      wasm-pack
    ];
  };
}
