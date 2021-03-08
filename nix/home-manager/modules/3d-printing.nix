{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
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
