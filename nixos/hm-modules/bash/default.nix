inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  home.packages = with custom; [
    emacsclient-remote
  ];
    
  home.file = {
    ".bashrc".source = ./.bashrc;
  };
}
