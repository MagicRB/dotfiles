inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }@rpkgs:
hm-path:
{ config, pkgs, ... }:
{
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  home-manager.users.main = rlib.callModule rpkgs hm-path;
}
