{ pkgs, config, lib, ... }:
with lib;
let
  wan = "enp7s0f1";
  lan = "enp7s0f0";
  doVPN = "do_vpn0";
in
{
  boot.kernel.sysctl = {
    # Enable forwarding on IPv4 but disable on IPv6
    "net.ipv4.conf.all.forwarding" = true;
    "net.ipv6.conf.all.forwarding" = false;

    # source: https://github.com/mdlayher/homelab/blob/master/nixos/routnerr-2/configuration.nix#L52
    # By default, not automatically configure any IPv6 addresses.
    "net.ipv6.conf.all.accept_ra" = 0;
    "net.ipv6.conf.all.autoconf" = 0;
    "net.ipv6.conf.all.use_tempaddr" = 0;

    # On WAN, allow IPv6 autoconfiguration and tempory address use.
    # "net.ipv6.conf.${name}.accept_ra" = 2;
    # "net.ipv6.conf.${name}.autoconf" = 1;
  };

  networking = {
    useDHCP = false;
    hostName = "blowhole";
    nameservers =  [ "8.8.8.8" ];
    # Disable the in-built iptable based firewall
    firewall.enable = mkForce false;

    interfaces = {
      # Don't do DHCP on the LAN interface
      "${lan}".useDHCP = false;
      # But do DHCP on the WAN interface
      "${wan}".useDHCP = true;
    };

    wireguard = {
      enable = true;
      interfaces."${doVPN}" =
        config.magic_rb.secret.wireguard."${config.networking.hostName}"
        // {
          listenPort = 6666;
          privateKeyFile = "/var/secrets/${doVPN}.key";
        };
    };

    nftables = {
      enable = true;
       ruleset = ''
        table ip filter {
          chain input_out {
            ct state { established, related } accept comment "Allow established traffic"
            icmp type { echo-request, destination-unreachable, time-exceeded } counter accept comment "Allow select ICMP"
          }

          chain input {
            type filter hook input priority 0; policy drop;

            tcp dport 22 accept comment "Accept SSH traffic always"

            iifname { "${lan}" } accept comment "Allow local network to access the router"
            iifname { "${wan}", "${doVPN}" } jump input_out
          }

          chain forward {
            type filter hook forward priority filter; policy drop;
            iifname { "${lan}" } oifname { "${wan}" } accept comment "Allow trusted LAN to WAN"
            iifname { "${wan}" } oifname { "${lan}" } ct state established, related accept comment "Allow established back to LANs"
          }
        }

        table ip nat {
          chain postrouting {
            type nat hook postrouting priority 100; policy accept;
            oifname "${wan}" masquerade
          }
        }

        table ip6 filter {
          chain input {
            type filter hook input priority 0; policy drop;
          }
          chain forward {
            type filter hook forward priority 0; policy drop;
          }
        }
      '';
    };
  };
}
