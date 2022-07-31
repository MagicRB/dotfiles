# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
with lib; {
  options.magic_rb.secret = mkOption {
    description = ''
      Magic_RB's secret sause.
    '';
    type = types.unspecified;
  };

  config = {
    _module.args.secret = config.magic_rb.secret;

    magic_rb.secret = let
      secret = "${inputs.secret}/default.nix";
      modules =
        evalModules
        {
          modules =
            [
              ./wireguard.nix
              ./network.nix
              ./password-hashes.nix
              ./mounts.nix
              ./emacs.nix
              ({...}: {_module.args.pkgs = pkgs;})
            ]
            ++ (
              if (builtins.tryEval {x = import secret;}).success
              then [secret]
              else builtins.trace "Warning! Not loading any secrets, you may get errors." []
            );
        };
    in
      modules.config;
  };
}
