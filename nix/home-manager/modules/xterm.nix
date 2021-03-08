{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  imports = [
    (import ./xresources { inherit nixpkgs nixpkgs-unstable nixpkgs-master custom; })
  ];
  
  home.packages = with nixpkgs; [
    xterm
  ];
}
