inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;
}
