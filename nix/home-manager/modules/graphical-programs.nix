{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    gimp
    firefox
    scrot
    xclip
    mpv
  ] ++ [
    nixpkgs-master.discord
  ];
}
