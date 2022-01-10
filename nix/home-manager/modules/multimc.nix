{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.programs.multimc;
  inherit (config.magic_rb.pkgs) nixpkgs-unstable;
in
{
  options.magic_rb.programs.multimc = {
    enable = mkEnableOption "Enable MultiMC Minecraft launcher.";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (nixpkgs-unstable.multimc.override { jdk = pkgs.jdk11; })
    ];
  };
}
