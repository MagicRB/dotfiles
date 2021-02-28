{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
{
  services.openssh.enable = true;
  
  environment.systemPackages = with nixpkgs; [
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
  
  imports = [
    ../modules/efi-grub.nix
    ../modules/nix-flakes.nix
    ../modules/pulseaudio.nix
    ../modules/xserver.nix
    ../modules/xkb-caps-us-sk.nix
  ];
}
