{ intelBusId, nvidiaBusId }:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
{
  hardware.bluetooth.enable = true;
  networking.networkmanager.enable = true;

  environment.systemPackages = with nixpkgs; [ libglvnd ];
 
  imports = [
    ./workstation.nix
    (import ../modules/xserver-prime.nix { inherit intelBusId nvidiaBusId; })
  ] ++ [
    
  ];
}
