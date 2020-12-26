{ config, pkgs, ... }:
{
  services.openssh.enable = true;
  
  environment.systemPackages = with pkgs; [
    lightdm
    home-manager
    cryptsetup
  ];
  
  imports = [
    ../modules/grub.nix
    ../modules/nix-flakes.nix
    ../modules/pulseaudio.nix
    ../modules/xserver.nix
    ../modules/xkb-caps-us-sk.nix
    ../modules/allow-unfree.nix
  ];
}
