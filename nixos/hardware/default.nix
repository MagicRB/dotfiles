# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.magic_rb.hardware;
in {
  options = {};

  imports = [
    ./heater.nix
    ./mark.nix
    ./omen.nix
    ./recovery-usb.nix
    (import ./oci-nixos.nix
      {
        hostName = "tweedledum";
        rootUUID = "79ba4403-7532-4e2c-ac5d-2910dce62009";
        efiUUID = "4478-6009";
        swapUUID = "d50e7ebf-8c62-4d2d-b19d-347378f7e5fe";
      })
    (import ./oci-nixos.nix
      {
        hostName = "tweedledee";
        rootUUID = "";
        efiUUID = "";
        swapUUID = "";
      })
    ./toothpick.nix
    ./gooseberry.nix
    ./blowhole.nix
  ];

  config = {
    assertions = [
      {
        assertion = let
          selection = mapAttrsToList (system: enabled: {inherit system enabled;}) cfg;
        in
          count (x: x.enabled) selection == 1;
        message = "You must select exactly one hardware configuration";
      }
    ];
  };
}
