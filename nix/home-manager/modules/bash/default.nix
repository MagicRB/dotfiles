{ pkgs, config, lib, ... }:
with lib;
let
  nglib = config.magic_rb.pins.nixng.lib pkgs.stdenv.system;
  cfg = config.magic_rb.programs.bash;
in
{
  options.magic_rb.programs.bash = {
    enable = mkEnableOption "Enable bash, the shell";
    emacsclient-remote = mkOption {
      description = "Enable emacsclient-remote and associated aliases";
      type = types.bool;
      default = true;
    };

    enableDirenv = mkEnableOption "Enable direnv";
  };
    
  config = mkIf cfg.enable {
    home.packages = mkMerge [
      (mkIf cfg.emacsclient-remote [
        pkgs.magic_rb.emacsclient-remote
      ])
      (mkIf cfg.enableDirenv [
        pkgs.direnv
      ])
    ];

    programs.direnv.enable = mkIf cfg.enableDirenv true;
    programs.direnv.enableNixDirenvIntegration = mkIf cfg.enableDirenv true;

    home.file = {
      ".bashrc".source = nglib.writeSubstitutedFile {
        name = ".bashrc";
        file = ./bashrc;
        substitutes = {
          "exa" = "${pkgs.exa}";
          "bat" = "${pkgs.bat}";
          "direnvEnabled" = if cfg.enableDirenv then "true" else "false";
        };
      };
    };
  };
}
