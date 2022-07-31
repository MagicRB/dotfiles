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
  cfg = config.magic_rb.packageCollections.cmdline;

  ffmpegSpecial =
    pkgs.ffmpeg_5-full.override
    {
      svt-av1 =
        pkgs.svt-av1.overrideAttrs
        (old: rec
          {
            version = "1.1.0";

            src = pkgs.fetchFromGitHub {
              owner = "AOMediaCodec";
              repo = "SVT-AV1";
              rev = "v${version}";
              sha256 = "sha256-A8PVrPQcsCx+cY0DKuvQ5g//1Iqk9+1Uvz6cN+Jc2E8=";
            };
          });
      # ffmpeg = pkgs.ffmpeg_5.overrideAttrs
      #   (old: rec
      #     { version = "5.1.0";

      #       src = pkgs.fetchFromGitHub {
      #         owner = "FFmpeg";
      #         repo = "FFmpeg";
      #         rev = "9222965fdd9594ff9e921d4ad25beac4eefa2373";
      #         sha256 = "sha256-MVCRynpG03pTDfSw7vhCxjDErSh84a7V5iM+zqr7P94=";
      #       };
      #     });
    };
in {
  options.magic_rb.packageCollections.cmdline = {
    enable =
      mkEnableOption
      ''
        A package collection containing command line programs, specifically zip, unzip, unrar (unfree), git, and htop.
      '';
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      zip
      unzip
      unrar
      git
      htop
      ffmpegSpecial
    ];
  };
}
