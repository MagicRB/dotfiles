{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    picom
  ];

  home.file = {
    ".config/picom.conf".source = ./picom.conf;
  };
}
