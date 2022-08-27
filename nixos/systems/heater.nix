# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  system = "x86_64-linux";
  name = "heater";
  module = {
    roots,
    inputs,
    pkgs,
    secret,
    lib,
    config,
    ...
  }:
    with lib; {
      imports = [
        (roots.nixos + "/profiles/workstation.nix")
      ];

      _module.args.nixinate = {
        host = "10.64.2.129";
        sshUser = "main";
        buildOn = "local";
        substituteOnTarget = true;
        hermetic = false;
        nixOptions = [
          "--override-input secret path://$HOME/dotfiles/secret"
        ];
      };

      nixpkgs.config.allowUnfree = true;

      home-manager.users."main" = {...}: {
        magic_rb = {
          optimisation.march = "znver2";
        };

        home.stateVersion = "20.09";
      };

      magic_rb = {
        optimisation.march = "znver2";
        grub = {
          enable = true;
          efi.enable = true;
        };

        xserver = {
          gpu = "nvidia";
        };

        erase-my-darlings.zfs = {
          enable = true;
          snapshot = "heater-zpool/local/root@blank";
        };

        hardware.heater = true;

        gaming.enable = true;
      };

      # Pinning
      nix.registry =
        flip mapAttrs inputs
        (
          n: flake: {inherit flake;}
        );

      networking = {
        hostName = "heater";
        useDHCP = false;
        interfaces.enp3s0.useDHCP = true;

        firewall.enable = true;
        hostId = "3457b383";

        firewall = {
          allowedTCPPorts = [22 25565];
        };
      };

      # System
      system.stateVersion = "20.09";
    };
}
