{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }: {
  home.packages = [
    nixpkgs-unstable.nixFlakes
  ];

  imports = [
    ../modules/bash
  ];
}
