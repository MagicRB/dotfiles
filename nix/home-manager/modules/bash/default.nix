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
  };
    
  config = mkIf cfg.enable {
    home.packages = mkIf cfg.emacsclient-remote [
      pkgs.magic_rb.emacsclient-remote
    ];

    home.file = {
      ".bashrc".source = nglib.writeSubstitutedFile {
        name = ".bashrc";
        file = ./bashrc;
        substitutes = {
          "exa" = "${pkgs.exa}";
          "bat" = "${pkgs.bat}";
        };
      };
    };
  };
}
