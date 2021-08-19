{ lib, config, ... }:
with lib;

{
  options.magic_rb.secret = mkOption {
    description = ''
      Magic_RB's secret sause.
    '';
    type = types.unspecified;
  };

  config = {
    _module.args.secret = config.magic_rb.secret;

    magic_rb.secret =
      let
        secret = "${config.magic_rb.pins.secret}/default.nix";
        modules = evalModules
          { modules =
              [ ./wireguard.nix
                ./network.nix
                ./password-hashes.nix
              ] ++
              (if (builtins.pathExists secret) then
                [ secret ]
               else
                 builtins.trace "Warning! Not loading any secrets, you may get errors." []
              );
          };
      in
        modules.config;
  };
}
