{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    picom
  ];

  home.file = {
    ".config/picom.conf".source = ./picom.conf;
  };
}
