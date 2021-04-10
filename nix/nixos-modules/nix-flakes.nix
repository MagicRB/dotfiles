{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.flakes;
in
{
  options.magic_rb.flakes = {
    enable = mkEnableOption "Enable flake support";
  };
  config = {
    nix = let
      nix = pkgs.nixFlakes;
    in {
      package = nix; 
      extraOptions = ''
        experimental-features = nix-command flakes ca-references
      '';
    };
  };
}
