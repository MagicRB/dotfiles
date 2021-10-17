{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.mounts;
in
{
  options.mounts = mkOption {
    type = with types; attrsOf (attrsOf unspecified);
    description = ''
      Mounts
    '';
  };
}
