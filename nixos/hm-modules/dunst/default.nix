inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    dunst
  ];

  home.file = {
    ".config/dunst/dunstrc".source = ./dunstrc;
  };
}
