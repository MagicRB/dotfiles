#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  system = "x86_64-linux";
  name = "blowhole";
  module = {
    pkgs,
    config,
    lib,
    secret,
    roots,
    inputs,
    ...
  }:
    with lib; {
      imports = [
        (roots.nixos + "/profiles/vps.nix")
        ./klipper.nix
        ./consul.nix
        ./nomad.nix
        ./vault.nix
        ./bind.nix
        ./vault-agent.nix
        ./nas.nix
        ./firewall.nix
        ./ical2org.nix
      ];

      home-manager.users."main" = {...}: {
        home.stateVersion = "21.05";
      };

      magic_rb = {
        grub = {
          enable = true;
          devices = [ "/dev/disk/by-id/ata-Apacer_AS350_128GB_2021020103001990" ];
        };

        hardware.blowhole = true;
      };

      _module.args.nixinate = {
        host = "10.64.2.1";
        sshUser = "main";
        buildOn = "local";
        substituteOnTarget = true;
        hermetic = false;
        nixOptions = [
          "--override-input secret path://$HOME/dotfiles/secret"
        ];
      };

      services.nfs.server = {
        enable = true;
        lockdPort = 4001;
        mountdPort = 4002;
        statdPort = 4000;
        exports = ''
          /var/nfs/jellyfin/cache             172.17.0.0/24(rw,subtree_check,async,no_root_squash,crossmnt)
          /var/nfs/jellyfin/config            172.17.0.0/24(rw,subtree_check,async,no_root_squash,crossmnt)
          /var/nfs/jellyfin/media             172.17.0.0/24(rw,subtree_check,async,no_root_squash,crossmnt)

          /var/nfs/gitea-data                 172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/gitea-db                   172.17.0.0/24(rw,subtree_check,async,no_root_squash)

          /var/nfs/hydra-data                 172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/hydra-nix                  172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/hydra-db                   172.17.0.0/24(rw,subtree_check,async,no_root_squash)

          /var/nfs/minecraft/atm6             172.17.0.0/24(rw,subtree_check,async,no_root_squash)

          /var/nfs/ingress-letsencrypt        10.64.0.1(rw,subtree_check,async,no_root_squash)

          /var/nfs/Magic_RB                   10.64.2.129(rw,subtree_check,async)
          /mnt/cartman                        10.64.2.129(rw,subtree_check,async,crossmnt)
          /mnt/kyle                           10.64.2.129(rw,subtree_check,async,crossmnt)
          /mnt/stan                           10.64.2.129(rw,subtree_check,async,crossmnt)

          /var/nfs/home-assistant_hass        172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/home-assistant_mosquitto   172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/home-assistant_zigbee2mqtt 172.17.0.0/24(rw,subtree_check,async,no_root_squash)

          /var/nfs/syncthing/data             172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/syncthing/config           172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/syncthing/storage          172.17.0.0/24(rw,subtree_check,async,no_root_squash)

          /var/nfs/dovecot/maildir            172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/getmail/getmail.d          172.17.0.0/24(rw,subtree_check,async,no_root_squash)

          /var/nfs/baikal/specific            172.17.0.0/24(rw,subtree_check,async,no_root_squash)
          /var/nfs/baikal/config              172.17.0.0/24(rw,subtree_check,async,no_root_squash)
        '';
      };

      # systemd.tmpfiles.rules = singleton "d /run/cfg/vault 0750 vault vault 1d";

      networking = {
        hostName = "blowhole";
        useDHCP = false;
        interfaces.enp7s0f1.useDHCP = true;

        firewall = {
          enable = true;

          allowedTCPPorts = [
            80
            ## Nomad
            4646
            4647
            4648
            ## Consul
            8600 # DNS
            8500 # HTTP
            8502 # gRPC
            8300 # server
            8301 # LAN serf
            8302 # WAN serf
            ## Vault
            8200
            ## NFS
            111
            2049
            4000
            4001
            4002
            20048
          ];
          allowedTCPPortRanges = [
            {
              from = 21000;
              to = 21999;
            }
          ];
          allowedUDPPorts = [
            ## Consul
            8600 # DNS
            8301 # LAN serf
            8302 # WAN serf
            ## NFS
            111
            2049
            4000
            4001
            4002
            20048
          ];
          allowedUDPPortRanges = [
            {
              from = 21000;
              to = 21999;
            }
          ];
        };
        hostId = "2cb135ac";
      };

      system.stateVersion = "21.05";
    };
}
