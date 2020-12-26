{ config, pkgs, ... }:
let
  mkIf = pkgs.lib.mkIf;
  bluetooth-enable = config.hardware.bluetooth.enable;
in {
  sound.enable = true;
  hardware = {
    pulseaudio = {
      enable = true;
      package = mkIf bluetooth-enable pkgs.pulseaudioFull;
    };
    pulseaudio.support32Bit = true;
  };

  systemd.user.services.pulseaudio.enable = true;
}
