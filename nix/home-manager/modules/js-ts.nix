{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = [
    nixpkgs.yarn
#    custom.yarn2nix
  ];
}
