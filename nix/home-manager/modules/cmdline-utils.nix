{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = with nixpkgs; [
    zip
    unzip
    pinentry
    libqrencode
    unrar
    pciutils
    git
    socat
    gnumake
    llvmPackages.bintools
    pkgconfig 
  ] ++ (with custom; [
    screenshot
  ]);
}
