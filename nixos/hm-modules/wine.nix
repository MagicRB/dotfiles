{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    winetricks
  ] ++ (with nixpkgs-unstable; [
    wineWowPackages.staging
  ]);
}
