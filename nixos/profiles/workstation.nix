inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }@ rpkgs:
{ config, pkgs, ... }:
{
  services.openssh.enable = true;
  
  environment.systemPackages = with pkgs; [
    home-manager
    cryptsetup
    nix-tree
    ntfs3g
    cifs-utils
  ];
  
  services.xserver = {
    displayManager = {
      lightdm.enable = true;
    };
  };
  
  imports = rlib.callModules rpkgs [
    ../modules/allow-unfree.nix
    ../modules/efi-grub.nix
    ../modules/nix-flakes.nix
    ../modules/pulseaudio.nix
    ../modules/xserver.nix
    ../modules/xkb-caps-us-sk.nix
  ];
}
