{ pkgs, config, lib, ... }:
with lib;
let
  lan = "enp7s0f0";
  wan = "enp7s0f1";
  doVPN = "do_vpn0";

  nomad = mapAttrs (const toString) {
    inherit (config.services.hashicorp.nomad.settings.client)
      min_dynamic_port
      max_dynamic_port;
  };
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

  services.stubby = {
    enable = true;
    settings = {
      resolution_type = "GETDNS_RESOLUTION_STUB";
      dns_transport_list = [
        "GETDNS_TRANSPORT_TLS"
      ];
      tls_authentication = "GETDNS_AUTHENTICATION_REQUIRED";
      tls_query_padding_blocksize = 256;
      edns_client_subnet_private = 1;
      idle_timeout = 10000;
      listen_addresses = [
        "127.0.0.1@5353"
      ];
      dnssec_return_status = "GETDNS_EXTENSION_TRUE";
      appdata_dir = "/var/cache/stubby";
      round_robin_upstreams = 1;
      upstream_recursive_servers = [
        {
          address_data = "9.9.9.9";
          tls_auth_name = "dns9.quad9.net";
          tls_pubkey_pinset = {
            digest = "sha384";
            value = "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEfYvXHQOFDRglszQcKaEn1KwBJUiKoPHqArnYUSwIaqxyVuz6Paagn0kJVY6s/rlzF1wC+3jMJJGUb0MjiQ4dZg==";
          };
        }
      ];
    };
  };

  services.dhcpd4 = {
    enable = true;
    interfaces = [ "${lan}" ];
    extraConfig = ''
        option domain-name-servers 10.64.2.1;
        option subnet-mask 255.255.255.0;

        subnet 10.64.2.0 netmask 255.255.255.0 {
          option broadcast-address 10.64.2.255;
          option routers 10.64.2.1;
          interface ${lan};
          range 10.64.2.128 10.64.2.254;
        }
      '';
  };

  networking = {
    useDHCP = false;
    hostName = "blowhole";
    nameservers =  [ "10.64.2.1" ];
    # Disable the in-built iptable based firewall
    firewall.enable = mkForce false;

    interfaces = {
      # Don't do DHCP on the LAN interface
      "${lan}" = {
        useDHCP = false;
        ipv4.addresses = [{
          address = "10.64.2.1";
          prefixLength = 24;
        }];
      };
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
          table ip nf_filter {
            chain input_out {
              ct state { established, related } accept comment "Allow established traffic"
              icmp type { echo-request, destination-unreachable, time-exceeded } counter accept comment "Allow select ICMP"
            }

            chain input_doVPN {
              tcp dport { 4646, 4647, 4648 } accept comment "Nomad traffic"
              tcp dport { 8600, 8500, 8502, 8300, 8301, 8302 } accept comment "Consul traffic"
              tcp dport { 8200 } accept comment "Vault traffic"
              tcp dport { 111, 2049, 4000, 4001, 4002, 20048 } accept comment "NFS traffic"
              tcp dport ${nomad.min_dynamic_port}-${nomad.max_dynamic_port} accept comment "Consul Connect sidecar traffic"

              udp dport { 8600, 8301, 8302 } comment "Consul traffic"
              udp dport { 111, 2049, 4000, 4001, 4002, 20048 } accept comment "NFS traffic"
              udp dport ${nomad.min_dynamic_port}-${nomad.max_dynamic_port} accept comment "Consul Connect sidecar traffic"
            }

            chain input {
              type filter hook input priority 0; policy drop;

              tcp dport 22 accept comment "Accept SSH traffic always"
              iifname != "lo" tcp dport 5353 drop comment "Drop traffic to stubby always except for localhost to localhost traffic"

              iifname "nomad" oifname "nomad" accept comment "Allow Nomad to do whatever it wants in its interface"
              iifname { "${lan}", "lo" } accept comment "Allow local network to access the router"
              iifname { "${wan}", "${doVPN}", "nomad", "docker0" } jump input_out
              iifname { "${doVPN}" } jump input_doVPN

              # Allow containers to reach the DNS server
              iifname { "nomad", "docker0" } tcp dport 53 accept
              iifname { "nomad", "docker0" } udp dport 53 accept

              # Allow containers to reach the NFS server
              iifname { "docker0" } tcp dport { 111, 2049, 4000, 4001, 4002, 20048 } accept comment "NFS traffic"
              iifname { "docker0" } udp dport { 111, 2049, 4000, 4001, 4002, 20048 } accept comment "NFS traffic"

              meta nftrace set 1
            }

            chain output {
              type filter hook input priority 0; policy accept;

              # Drop all DNS traffic if leaving through "wan"
              oifname { "${wan}" } tcp dport 53 drop
              oifname { "${wan}" } udp dport 53 drop
              # Allow DoT traffic to leave through "wan" if it comes from "lo"
              iifname != { "lo" } oifname { "${wan}" } tcp dport 853 drop
            }

            chain forward {
              type filter hook forward priority 10; policy drop;

              # Enable flow offloading for better throughput
              # ip protocol { tcp, udp } flow offload @f

              # Drop all DNS or DoT traffic if forwarded through "wan"
              oifname { "${wan}" } tcp dport 853 drop
              oifname { "${wan}" } tcp dport 53 drop
              oifname { "${wan}" } udp dport 53 drop

              # Allow trusted LAN to WAN"
              iifname { "${lan}" } oifname { "${wan}" } accept
              iifname { "${wan}" } oifname { "${lan}" } ct state established, related accept


              iifname { "nomad" } oifname { "${doVPN}", "${lan}" } accept
              iifname { "${doVPN}", "${lan}" } oifname { "nomad" } accept
              iifname { "${doVPN}" } oifname { "${lan}" } accept
              iifname { "${lan}" } oifname { "${doVPN}" } accept

              # Allow containers to reach WAN
              iifname { "nomad", "docker0" } oifname { "${wan}" } accept
              iifname { "${wan}" } oifname { "nomad", "docker0" } ct state established, related accept

              # Allow containers to reach the DNS and NFS server
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip daddr 10.64.2.1 tcp dport { 53 } accept
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip saddr 10.64.2.1 tcp sport { 53 } accept
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip daddr 10.64.2.1 tcp dport { 111, 2049, 4000, 4001, 4002, 20048 } accept
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip saddr 10.64.2.1 tcp sport { 111, 2049, 4000, 4001, 4002, 20048 } accept
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip daddr 10.64.2.1 udp dport { 53 } accept
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip saddr 10.64.2.1 udp sport { 53 } accept
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip daddr 10.64.2.1 udp dport { 111, 2049, 4000, 4001, 4002, 20048 } accept
              iifname { "nomad", "docker0" } oifname { "${lan}" } ip saddr 10.64.2.1 udp sport { 111, 2049, 4000, 4001, 4002, 20048 } accept


              # Rules to make CNI happy
              meta mark and 0x01 == 0x01 accept

              meta nftrace set 1
            }
          }

          table ip nf_nat {
            chain postrouting {
              type nat hook postrouting priority 100; policy accept;
              oifname "${wan}" masquerade
            }

            chain prerouting {
              type nat hook prerouting priority 100; policy accept;
            }
          }

          table ip6 nf_filter {
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

  systemd.services.nftables = {
    serviceConfig =
      let
        rulesScript = pkgs.writeShellScript "nftables-rules" ''
          set -ex
          export PATH=${pkgs.nftables}/bin:${pkgs.iptables}/bin:${pkgs.bash}/bin:$PATH

          tmpfile="$(mktemp)"
          iptables-save -t filter >> $tmpfile
          iptables-save -t nat >> $tmpfile

          nft flush ruleset

          cat $tmpfile | iptables-restore
          nft -f "${config.networking.nftables.rulesetFile}"
          rm $tmpfile

          iptables -D FORWARD -j MARK --set-mark 0x01 || true
          iptables -D FORWARD -j MARK --set-mark 0x00 || true

          iptables -I FORWARD -j MARK --set-mark 0x01
          iptables -A FORWARD -j MARK --set-mark 0x00
        '';
      in {
        ExecStart = mkForce rulesScript;
        ExecReload = mkForce rulesScript;
        ExecStop = mkForce (pkgs.writeShellScript "nftables-flush" ''
          set -ex
          export PATH=${pkgs.nftables}/bin:${pkgs.iptables}/bin:${pkgs.bash}/bin:$PATH

          tmpfile="$(mktemp)"
          iptables-save -t filter >> $tmpfile
          iptables-save -t nat >> $tmpfile

          nft flush ruleset

          cat $tmpfile | iptables-restore
          rm $tmpfile

          iptables -D FORWARD -j MARK --set-mark 0x01 || true
          iptables -D FORWARD -j MARK --set-mark 0x00 || true

          iptables -I FORWARD -j MARK --set-mark 0x01
          iptables -A FORWARD -j MARK --set-mark 0x00
        '');
      };
  };
}

