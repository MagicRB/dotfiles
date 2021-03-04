inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
{
  nix.registry = {
    "nixpkgs".flake = inputs.nixpkgs;
    "nixpkgs-unstable".flake = inputs.nixpkgs-unstable;
    "nixpkgs-master".flake = inputs.nixpkgs-master;
  };

  nix.nixPath = [
    "nixpkgs=${inputs.nixpkgs}"
    "nixpkgs-unstable=${inputs.nixpkgs-unstable}"
    "nixpkgs-master=${inputs.nixpkgs-master}"
  ];

  nixpkgs.pkgs = nixpkgs;
}
