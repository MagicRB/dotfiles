# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  system = "x86_64-linux";
  name = "toothpick";
  module = {
    lib,
    pkgs,
    config,
    secret,
    inputs,
    roots,
    self,
    ...
  }:
with lib;
{
      imports = [
        (roots.nixos + "/modules")
        ./consul.nix
        ./nomad.nix
        ./vault-agent.nix
      ];

      _module.args.nixinate = {
        host = "redalder.org";
        sshUser = "main";
        buildOn = "local";
        substituteOnTarget = true;
        hermetic = false;
      };

      magic_rb = {
        hardware.toothpick = true;
        flakes.enable = true;
        sshdEmacs.enable = true;
        vpsRemoteAccess = {
          enable = true;
          trustedWheel = true;
        };
      };
      boot.loader.grub = {
        enable = true;
        version = 2;
        efiSupport = false;
      };

      environment.systemPackages = [
        pkgs.git
      ];

      boot.kernel.sysctl = {"net.ipv4.ip_forward" = "1";};

      # https://github.com/NixOS/nixpkgs/issues/76671
      # the rpc.statd daemon is not running when not mounting any nfs filesystems on boot
      # and can't be manually started...
      services.nfs.server.enable = true;

      networking = {
        hostName = "toothpick";

        nameservers = [
          "${secret.network.ips.blowhole.ip}"
          "93.184.77.2"
          "67.207.67.3"
        ];

        wireguard = {
          enable = true;
          interfaces."wg0" =
            {
              postSetup = ''
                ${pkgs.iptables}/bin/iptables -I FORWARD -i wg0 -o wg0 -j ACCEPT
              '';

              postShutdown = ''
                ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -o wg0 -j ACCEPT
              '';
            }
            // config.magic_rb.secret.wireguard."toothpick";
        };

        defaultGateway = "64.225.96.1";
        defaultGateway6 = "";
        dhcpcd.enable = false;
        usePredictableInterfaceNames = lib.mkForce false;

        firewall = {
          extraCommands = ''
            iptables -P FORWARD DROP
          '';

          # extraStopCommands = ''
          # '';

          interfaces."eth0" = {
            allowedTCPPorts = [
              80
              443
            ];
            allowedUDPPorts = [
              6666
            ];
          };

          interfaces."wg0" = {
            allowedTCPPorts = [
              ## Consul
              8600 # DNS
              8500 # HTTP
              8502 # gRPC
              8300 # server
              8301 # LAN serf
              8302 # WAN serf
              4646 # Nomad
              4647
              4648
              10000
            ];
            allowedTCPPortRanges = [
              {
                from = 21000;
                to = 21255;
              }
            ];
            allowedUDPPorts = [
              ## Consul
              8600 # DNS
              8301 # LAN serf
              8302 # WAN serf
            ];
            allowedUDPPortRanges = [
              {
                from = 21000;
                to = 21255;
              }
            ];
          };
        };

        interfaces = {
          eth0 = {
            ipv4.addresses = [
              {
                address = "64.225.104.221";
                prefixLength = 20;
              }
              {
                address = "10.19.0.6";
                prefixLength = 16;
              }
            ];
            ipv6.addresses = [
              {
                address = "fe80::8ce0:84ff:fefb:f981";
                prefixLength = 64;
              }
            ];
            ipv4.routes = [
              {
                address = "64.225.96.1";
                prefixLength = 32;
              }
            ];
          };
        };
      };

      security.pki.certificates =
        singleton (builtins.readFile (roots.flake + "/redalder.org.crt"));

      services.udev.extraRules = ''
        ATTR{address}=="8e:e0:84:fb:f9:81", NAME="eth0"
      '';

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "21.05";
    };
}
