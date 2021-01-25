inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  services.gpg-agent = {
    pinentryFlavor = "gtk2";
    enable = true;
  };
  programs.gpg.enable = true;

  home.packages = with nixpkgs; [
    zip
    unzip
    pinentry
    libqrencode
    unrar
    exa
    bat
    pciutils
    git
    socat
    gnumake
    hugo
    llvmPackages.bintools
    pkgconfig
    nixpkgs-unstable.wineWowPackages.staging
    winetricks
  ];
}
