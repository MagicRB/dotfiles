inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, pkgs, ... }:
{
  nix.registry = {
    "nixpkgs".flake = inputs.nixpkgs;
    "nixpkgs-unstable".flake = inputs.nixpkgs-unstable;
    "nixpkgs-master".flake = inputs.nixpkgs-master;
  };
}
