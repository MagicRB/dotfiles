{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.file = {
    ".Xresources".source = ./.Xresources;
  };
}
