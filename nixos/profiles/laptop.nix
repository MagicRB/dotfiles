{ intelBusId, nvidiaBusId }: { config, pkgs, ... }:
{
  hardware.bluetooth.enable = true;
  networking.networkmanager.enable = true;

  environment.systemPackages = with pkgs; [ libglvnd ];
 
  imports = [
    ./workstation.nix
    (import ../modules/xserver-prime.nix { inherit intelBusId nvidiaBusId; })
  ];
}
