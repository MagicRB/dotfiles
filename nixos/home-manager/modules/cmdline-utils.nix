{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom }:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    zip
    unrar
    cargo
    exa
    bat
    pciutils
    git
    socat
    nixpkgs-unstable.bfs
  ];
}
