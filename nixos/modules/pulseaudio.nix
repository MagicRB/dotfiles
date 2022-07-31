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
    sound.enable = true;
    hardware = {
      pulseaudio = {
        enable = true;

        package = mkIf bluetooth-enable pkgs.pulseaudioFull;
        daemon.config = {
          default-sample-rate = 96000;
          default-sample-format = "s32le";
          resample-method = "speex-float-5";
          nice-level = -19;
        };
      };
      pulseaudio.support32Bit = true;
    };

    systemd.user.services.pulseaudio.enable = true;
  };
}
