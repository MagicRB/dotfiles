{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.file = {
    ".Xresources".source = ./.Xresources;
  };
}
