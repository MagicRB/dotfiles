{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  imports = [
    (import ./xresources { inherit nixpkgs nixpkgs-unstable nixpkgs-master custom; })
  ];
  
  home.packages = with nixpkgs; [
    xterm
  ];
}
