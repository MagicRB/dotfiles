{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, ... }:
{ config, lib, pkgs, ... }:
{
  home.packages = with custom; [
    emacsclient-remote
  ];
    
  home.file = {
    ".bashrc".source = ./.bashrc;
  };
}
