{ multimc5 ? false
, wine ? false
, _3dPrinting ? false
, js-ts ? false }:
{ config, pkgs, ... }:
let
  inherit (config.magic_rb.pins) nixpkgs-unstable;
  inherit (pkgs) lib;

in
{
  home.packages = [
    nixpkgs-unstable.nomad
    pkgs.magic_rb.sss-cli

  ] ++ (with pkgs; [
    fira-code
    overpass
  ]);

  home.stateVersion = "20.09";

  imports = [
    ../modules/alacritty
    ../modules/bash
    ../modules/cmdline-utils.nix
    ../modules/dunst
    ../modules/emacs
    ../modules/graphical-programs.nix

    ../modules/webdev.nix
    ../modules/rust.nix

    ../modules/nix-du.nix
    ../modules/picom

    ../modules/urxvt.nix
    ../modules/xmonad

    ../modules/gpg.nix
  ];
  # ++ (lib.optionals multimc5 [ ../modules/multimc.nix ])
  # ++ (lib.optionals wine [ ../modules/wine.nix ])
  # ++ (lib.optionals _3dPrinting [ ../modules/3d-printing.nix ])
  # ++ (lib.optionals js-ts [ ../modules/js-ts.nix ]);
}
