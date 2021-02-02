{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    powerline-fonts
    font-awesome
    dejavu_fonts
  ];

  home.file.".xmonad/xmonad.hs".source = let
    vars = {
      xmobar = "${nixpkgs.xmobar}/bin/xmobar";
      xmobarConfig = ./xmobarrc;
      screenshot = "${custom.screenshot}/bin/screenshot";
    };
  in with rlib; substitute nixpkgs.runCommandNoCCLocal "xmonad.hs" ./xmonad.hs vars;
}
