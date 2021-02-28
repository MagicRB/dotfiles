{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    gimp
    steam
    firefox
    scrot
    xclip
    discord
    mpv
  ];
}