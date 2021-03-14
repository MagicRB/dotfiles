{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, ... }:
{
  hardware.bluetooth.enable = true;
  networking.networkmanager.enable = true;

  environment.systemPackages = with nixpkgs; [
    libglvnd
  ];
 
  imports = [
    ./workstation.nix
  ] ++ [
    
  ];
}
