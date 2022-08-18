# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  bluetooth-enable = config.hardware.bluetooth.enable;
  cfg = config.magic_rb.pulseaudio;
in {
  options.magic_rb.pulseaudio = {
    enable = mkEnableOption "Enable pulseaudio, with bluetooth support if bt enabled";
  };

  config = mkIf cfg.enable {
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };
  };
}
