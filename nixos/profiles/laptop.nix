{ intelBusId, nvidiaBusId }:
inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }@ rpkgs:
{ config, pkgs, ... }:
{
  hardware.bluetooth.enable = true;
  networking.networkmanager.enable = true;

  environment.systemPackages = with pkgs; [ libglvnd ];
 
  imports = rlib.callModules rpkgs [
    ./workstation.nix
  ] ++ [
    (import ../modules/xserver-prime.nix { inherit intelBusId nvidiaBusId; } inputs rpkgs)
  ];
}
