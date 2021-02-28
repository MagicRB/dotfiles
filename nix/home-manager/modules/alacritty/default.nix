{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    alacritty
  ];

  home.file = {
    ".config/alacritty/alacritty.yaml".source = ./alacritty.yaml;
  };
}
