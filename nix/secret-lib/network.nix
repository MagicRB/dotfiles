{ lib, ... }:
with lib;

{
  options = {
    network.ips = mkOption {
      description = ''
        Host IPs.
      '';
      type = with types; attrsOf (oneOf [ str (attrsOf str) ]);
      default = {};
    };

    network.networks = mkOption {
      description = ''
        Network IPs.
      '';
      type = with types; attrsOf str;
      default = {};
    };
  };
}
