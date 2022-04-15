{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.flakes;
in
{
  options.magic_rb.flakes = {
    enable = mkEnableOption "Enable flake support";
    nixMaster = mkEnableOption "Whether to enable Nix master for dwarffs.";
  };
  config = mkIf cfg.enable {
    nixpkgs.overlays = mkIf cfg.nixMaster [ config.magic_rb.pins.nix.overlay ];

    nix =
      {
        package =
          if !cfg.nixMaster then
            pkgs.nixFlakes
          else
            pkgs.nix;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };
  };
}
