inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    picom
  ];

  home.file = {
    ".config/picom.conf".source = ./picom.conf;
  };
}
