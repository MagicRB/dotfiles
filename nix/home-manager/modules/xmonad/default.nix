{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    powerline-fonts
    font-awesome
    dejavu_fonts
  ];

  home.file.".xmonad/xmonad.hs".source = rlib.substitute {
    runCommand = nixpkgs.runCommandNoCCLocal;
    name = "xmonad.hs";
    inFile = ./xmonad.hs;
    vars = {
      xmobar = "${nixpkgs.xmobar}/bin/xmobar";
      xmobarConfig = ./xmobarrc;
      screenshot = "${custom.screenshot}/bin/screenshot";
      dmenu_run = "${nixpkgs.dmenu}/bin/dmenu_run";
    };
  };
}
