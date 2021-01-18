{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, ... }:
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
    cura
    inkscape
  ];
}
