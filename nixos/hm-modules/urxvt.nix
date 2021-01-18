inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }@ rpkgs:
{ config, lib, pkgs, ... }:
{
  imports = rlib.callModules rpkgs [
    ./xresources
  ];

  home.packages = with nixpkgs; [
    rxvt-unicode
  ];
}
