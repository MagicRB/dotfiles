{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, ... }:
{
  services.xserver = {
    enable = true;

    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
    };

    displayManager = {
      defaultSession = "none+xmonad";
    };
    
    videoDrivers = [ "nvidia" ];
    libinput.enable = true;
  };

  hardware = {
    opengl.driSupport32Bit = true;
    # opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ]; # What does this do??
  };
}
