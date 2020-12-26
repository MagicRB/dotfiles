{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom }:
{ config, lib, pkgs, ... }:
{
  home.file = {
    ".Xresources".source = ./.Xresources;
  };
}
