{ multimc5 ? false
, wine ? false
, _3dPrinting ? false }:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }: {
  home.packages = [
    nixpkgs-unstable.nomad_1_0
    custom.sss-cli

  ] ++ (with nixpkgs; [
    fira-code
    overpass
  ]);

  home.stateVersion = "20.09";

  programs.gpg.enable = true;

  imports = [
    ../hm-modules/alacritty
    ../hm-modules/bash
    ../hm-modules/cmdline-utils.nix
    ../hm-modules/dunst
    ../hm-modules/emacs
    ../hm-modules/graphical-programs.nix

    ../hm-modules/webdev.nix
    ../hm-modules/rust.nix

    ../hm-modules/nix-du.nix
    ../hm-modules/picom

    ../hm-modules/urxvt.nix
    ../hm-modules/xmonad
  ]
  ++ (lib.optionals multimc5 [ ../hm-modules/multimc.nix ])
  ++ (lib.optionals wine [ ../hm-modules/wine.nix ])
  ++ (lib.optionals _3dPrinting [ ../hm-modules/3d-printing.nix ]);
}
