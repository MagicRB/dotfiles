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
  cfg = config.magic_rb.programs.multimc;
  inherit (config.magic_rb.pkgs) nixpkgs-unstable;

  # @TODO: remove
  nixpkgs-polymc =
    pkgs.libsForQt5.callPackage
    ((pkgs.fetchFromGitHub
      {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "68c360c518091f0e2d355dd3aa7186f4e8da0935";
        sha256 = "sha256-74xEjIRjwswTNPrupFR4IAcT5k1z+T1tRrskhEs0Z+4=";
      })
    + "/pkgs/games/polymc/default.nix")
    {jdks = with pkgs; [jdk11 jdk8 jdk17 graalvm17-ce];};
in {
  options.magic_rb.programs.multimc = {
    enable = mkEnableOption "Enable MultiMC Minecraft launcher.";
  };

  config = mkIf cfg.enable {
    home.packages = [
      nixpkgs-polymc
    ];
  };
}
