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
}
