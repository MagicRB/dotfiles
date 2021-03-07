{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = [
    nixpkgs.yarn
    custom.yarn2nix
  ];
}
