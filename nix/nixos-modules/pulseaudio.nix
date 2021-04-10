{ config, lib, pkgs, ... }:
with lib;
let
  bluetooth-enable = config.hardware.bluetooth.enable;
  cfg = config.magic_rb.pulseaudio;
in
{
  options.magic_rb.pulseaudio = {
    enable = mkEnableOption "Enable pulseaudio, with bluetooth support if bt enabled";
  };

  config = mkIf cfg.enable {
    sound.enable = true;
    hardware = {
      pulseaudio = {
        enable = true;
        package = mkIf bluetooth-enable pkgs.pulseaudioFull;
      };
      pulseaudio.support32Bit = true;
    };

    systemd.user.services.pulseaudio.enable = true;
  };
}
