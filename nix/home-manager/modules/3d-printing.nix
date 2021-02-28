{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    freecad
    openscad
    cura
    inkscape
    
  ] ++ [
    nixpkgs-unstable.prusa-slicer
  ];
}
