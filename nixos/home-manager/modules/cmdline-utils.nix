{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom }:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    zip
    unrar
    exa
    bat
    pciutils
    git
  ];
}
