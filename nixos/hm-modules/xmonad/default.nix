inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
hostname:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    powerline-fonts
    font-awesome
    dejavu_fonts
  ];

  home.file.".xmonad/xmonad.hs".source = let
    vars = {
      xmobar = rlib.binPath nixpkgs.xmobar "xmobar";
      xmobarConfig = ./xmobarrc;
      screenshot = rlib.binPath custom.screenshot "screenshot";
    };
  in with rlib; substitute nixpkgs.runCommandNoCCLocal "xmonad.hs" ./xmonad.hs vars;
}
