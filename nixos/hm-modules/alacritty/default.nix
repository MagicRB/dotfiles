inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    alacritty
  ];

  home.file = {
    ".config/alacritty/alacritty.yaml".source = ./alacritty.yaml;
  };
}
