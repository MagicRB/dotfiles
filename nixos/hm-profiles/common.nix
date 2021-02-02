{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }: {
  home.packages = [
    # custom.enter-env
    nixpkgs-unstable.nomad_1_0
    custom.sss-cli
    custom.enter-env

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
  ];
}
