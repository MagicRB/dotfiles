{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }: {
  home.packages = [
    nixpkgs-unstable.nixFlakes
  ];

  imports = [
    ../hm-modules/bash
  ];
}
