{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.magic_rb.programs.shh;
in
{
  options.magic_rb.programs.shh = {
    enable = mkEnableOption "Enable shh, the Haskell shell.";
  };

  config = mkIf cfg.enable {
    home.file.".shh/Shell.hs".source = ./Shell.hs;
    home.file.".shh/init.ghci".source = ./init.ghci;

    home.packages = [
      pkgs.magic_rb.shh
    ];
  };
}
