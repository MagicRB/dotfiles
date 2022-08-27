# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  system = "x86_64-linux";
  name = "omen";
  module = {
    pkgs,
    lib,
    config,
    secret,
    roots,
    inputs,
    ...
  }:
    with lib; {
      imports = [
        (roots.nixos + "/profiles/workstation.nix")
      ];

      _module.args.nixinate = {
        host = "10.64.0.8";
        sshUser = "main";
        buildOn = "local";
        substituteOnTarget = true;
        hermetic = false;
        nixOptions = [
          "--override-input secret path://$HOME/dotfiles/secret"
        ];
      };

      home-manager.users."main" = {...}: {
        magic_rb = {
          optimisation.march = "skylake";
        };

        services.syncthing.enable = true;

        home.stateVersion = "20.09";
      };

      specialisation.nvidia-sync = {
        configuration = {
          magic_rb.xserver.nvidia = {
            primeSync = true;
            primeOffload = lib.mkForce false;
          };
        };
        inheritParentConfig = true;
      };

      magic_rb = {
        optimisation.march = "skylake";
        grub = {
          enable = true;
          efi.enable = true;
        };

        xserver = {
          gpu = "nvidia";
          nvidia = {
            primeOffload = true;

            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          };
        };

        gaming.enable = true;
        hardware.omen = true;
        networking = {
          bluetooth = true;
          networkManager = true;
        };
      };

      # Networking
      networking = {
        hostName = "omen";
        useDHCP = false;
        interfaces.eno1.useDHCP = true;
        hostId = "10c7ffc5";

        firewall.allowedTCPPorts = [22000];

        wireguard.interfaces."wg0" =
          {} // config.magic_rb.secret.wireguard."omen";
      };

      # System
      system.stateVersion = "20.09";
    };
}
