{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  imports = [
    ./xresources
  ];

  home.packages = with nixpkgs; [
    rxvt-unicode
  ];
}
