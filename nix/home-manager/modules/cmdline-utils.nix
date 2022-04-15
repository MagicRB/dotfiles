{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.packageCollections.cmdline;


  ffmpegSpecial = pkgs.ffmpeg-full.override
    { svt-av1 = pkgs.svt-av1.overrideAttrs
      (old: rec
      { version = "1.0.0-rc1";

        src = pkgs.fetchFromGitHub {
          owner = "AOMediaCodec";
          repo = "SVT-AV1";
          rev = "v${version}";
          sha256 = "sha256-iN4ryhoMNlgXWiCup9pNucQKBkmZtC9bj3bsq8l9KYk=";
        };
      });
    };
in
{
  options.magic_rb.packageCollections.cmdline = {
    enable = mkEnableOption
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
