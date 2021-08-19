{ lib, ... }:
with lib;

{
  options = {
    passwordHashes = mkOption {
      description = ''
        Password hashes.
      '';
      type = with types; attrsOf (oneOf [ str (attrsOf str) ] );
      default = {};
    };
  };
}
