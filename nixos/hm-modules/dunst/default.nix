{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    dunst
  ];

  home.file = {
    ".config/dunst/dunstrc".source = ./dunstrc;
  };
}
