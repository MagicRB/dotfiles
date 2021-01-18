inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  imports = [
    (import ./xresources { inherit nixpkgs nixpkgs-unstable nixpkgs-master custom; })
  ];
  
  home.packages = with nixpkgs; [
    xterm
  ];
}
