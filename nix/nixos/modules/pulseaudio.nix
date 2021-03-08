{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, ... }:
let
  mkIf = nixpkgs.lib.mkIf;
  bluetooth-enable = config.hardware.bluetooth.enable;
in {
  sound.enable = true;
  hardware = {
    pulseaudio = {
      enable = true;
      package = mkIf bluetooth-enable nixpkgs.pulseaudioFull;
    };
    pulseaudio.support32Bit = true;
  };

  systemd.user.services.pulseaudio.enable = true;
}
