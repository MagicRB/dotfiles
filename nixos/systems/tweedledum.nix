# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  system = "x86_64-linux";
  name = "tweedledum";
  module = {
    pkgs,
    config,
    inputs,
    roots,
    ...
  }: {
    imports = [
      (roots.nixos + "/profiles/oracle-cloud.nix")
    ];

    home-manager.users."main" =
      {...}:
      {
        home.stateVersion = "22.05";
      };

    _module.args.nixinate = {
      host = "tweedledum.redalder.org";
      sshUser = "main";
      buildOn = "local";
      substituteOnTarget = true;
      hermetic = false;
    };

    magic_rb.hardware.tweedledum = true;

    networking.hostName = "tweedledum";
    system.stateVersion = "20.09";
  };
}
