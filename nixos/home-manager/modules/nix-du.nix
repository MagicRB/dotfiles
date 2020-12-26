{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom }:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    nix-du
    graphviz
  ];
}
