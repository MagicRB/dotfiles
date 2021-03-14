{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.pins;
in
{
  options.magic_rb.pins = mkOption {
    description = "nix things to pin into the PATH and registry";
    type = types.attrsOf types.unspecified;
  };

  config.nix = mkMerge (mapAttrsToList (name: value: {
    registry."${name}".flake = value;
    nixPath = [ "${name}=${value}" ];
  }) cfg);    
}
