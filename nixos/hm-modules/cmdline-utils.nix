{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
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
