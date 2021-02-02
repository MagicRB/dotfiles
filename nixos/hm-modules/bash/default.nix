{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with custom; [
    emacsclient-remote
  ];
    
  home.file = {
    ".bashrc".source = ./.bashrc;
  };
}
