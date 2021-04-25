{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.programs.ssh;
in
{
  options.magic_rb.programs.ssh = {
    enable = mkEnableOption "Enable ssh_config";
  };

  config = mkIf cfg.enable {
    home.file = {
      ".ssh/config".source = ./config;
    };

    home.activation."ssh-controlmasters" = config.lib.dag.entryAfter ["writeBoundary"] ''
      mkdir -p ~/.ssh/controlmasters
    '';
  };
}
