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
    ssss
    unrar
    cargo
    exa
    bat
    pciutils
    git
    socat
    gnumake
    nixpkgs-unstable.bfs
    hugo
    llvmPackages.bintools
    pkgconfig
  ];
}
