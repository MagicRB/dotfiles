{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    winetricks
  ] ++ (with nixpkgs-unstable; [
    wineWowPackages.staging
  ]);
}
