inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    gimp
    steam
    firefox
    scrot
    xclip
    discord
    mpv
    nixpkgs.freecad
    inkscape
  ];
}
