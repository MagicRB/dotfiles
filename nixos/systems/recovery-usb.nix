# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  system = "x86_64-linux";
  name = "recovery-usb";
  module = {
    pkgs,
    inputs,
    roots,
    ...
  }: {
    imports = [
      (roots.nixos + "/modules")
      inputs.home-manager.nixosModules.home-manager
      "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    ];
    home-manager.users."main" = {...}: {
      imports = [(roots.home-manager + "/modules")];

      magic_rb = {
        programs = {
          bash.enable = true;
          ssh.enable = true;
        };
      };

      home.stateVersion = "20.09";
    };
    magic_rb = {
      hardware.recoveryUsb = true;
      flakes.enable = true;
    };
  };
}
