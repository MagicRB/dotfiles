{ config, pkgs, ... }:
{
  services.openssh.enable = true;
  
  environment.systemPackages = with pkgs; [
    home-manager
    cryptsetup
  ];
  
  services.xserver = {
    displayManager = {
      lightdm.enable =true;
    };
  };
  
  imports = [
    ../modules/grub.nix
    ../modules/nix-flakes.nix
    ../modules/pulseaudio.nix
    ../modules/xserver.nix
    ../modules/xkb-caps-us-sk.nix
    ../modules/allow-unfree.nix
  ];
}
