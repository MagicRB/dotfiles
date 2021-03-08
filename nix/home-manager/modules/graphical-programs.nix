{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
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
