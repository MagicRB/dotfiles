{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.emacs;
in
{
  options.emacs = {
    mbsyncrc = mkOption {
      type = types.path;
    };

    mu4eContexts = mkOption {
      type = types.path;
    };
  };
}
