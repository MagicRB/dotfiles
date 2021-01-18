inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }@ rpkgs:
{ config, lib, pkgs, ... }: {
  home.packages = [
    # custom.enter-env
    nixpkgs-unstable.nomad_1_0
    custom.sss-cli
  ];

  home.stateVersion = "20.09";

  imports = rlib.callModules rpkgs [
    ../hm-modules/alacritty
    ../hm-modules/bash
    ../hm-modules/cmdline-utils.nix
    ../hm-modules/dunst
    ../hm-modules/emacs
    ../hm-modules/graphical-programs.nix

    ../hm-modules/webdev.nix

    #../hm-modules/i3
    ../hm-modules/nix-du.nix
    ../hm-modules/picom

    ../hm-modules/urxvt.nix
  ] ++ [
    (rlib.callModule rpkgs ../hm-modules/i3 "heater")
  ];
}
