inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
{
  nix.registry = {
    "nixpkgs".flake = inputs.nixpkgs;
    "nixpkgs-unstable".flake = inputs.nixpkgs-unstable;
    "nixpkgs-master".flake = inputs.nixpkgs-master;
  };
}
