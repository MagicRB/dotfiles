{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    alacritty
  ];

  home.file = {
    ".config/alacritty/alacritty.yaml".source = ./alacritty.yaml;
  };
}
