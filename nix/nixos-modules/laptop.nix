{ config, pkgs, ... }:
{
  hardware.bluetooth.enable = true;
  networking.networkmanager.enable = true;
}
