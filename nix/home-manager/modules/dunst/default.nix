{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    dunst
  ];

  home.file = {
    ".config/dunst/dunstrc".source = ./dunstrc;
  };
}
