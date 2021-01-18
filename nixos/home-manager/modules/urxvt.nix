{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, ... }:
{ config, lib, pkgs, ... }:
{
  imports = [
    (import ./xresources { inherit nixpkgs nixpkgs-unstable nixpkgs-master custom; })
  ];

  home.packages = with nixpkgs; [
    rxvt-unicode
  ];
}
