{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.magic_rb.optimisation;
in
{
  options.magic_rb.optimisation = {
    march = mkOption {
      description = ''
        Which march is native to this system, to maximize efficiency.
      '';
      type = types.str;
      default = builtins.trace ''
        Please enable a more specific march to get better performance
      '' "x86-64";
    };
  };
}
