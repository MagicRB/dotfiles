{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;

    windowManager = {
      i3.enable = true;
    };

    displayManager = {
      defaultSession = "none+i3";
    };
    
    videoDrivers = [ "nvidia" ];
    libinput.enable = true;
  };

  hardware = {
    opengl.driSupport32Bit = true;
    # opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ]; # What does this do??
  };
}
