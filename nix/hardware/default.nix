{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.magic_rb.hardware;
in
{
  options = {};

  imports = [ ./heater.nix ./mark.nix ./omen.nix ./recovery-usb.nix ];

  config = {
    assertions = [
      {
        assertion =
          let
            selection = mapAttrsToList (system: enabled: { inherit system enabled; }) cfg;
          in
            count (x: x.enabled) selection == 1;
        message = "You must select exactly one hardware configuration";
      }
    ];
  };
}
