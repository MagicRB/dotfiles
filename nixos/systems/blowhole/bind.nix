{ pkgs, ... }:
{
  services.bind = {
    enable = true;
    forwarders = [
      "8.8.8.8"
      "8.8.4.4"
    ];
    zones = {
      "vault.in.redalder.org" = {
        file = ./zones/vault.in.redalder.org.zone;
        master = true;
      };
      "hosts.in.redalder.org" = {
        file = ./zones/hosts.in.redalder.org.zone;
        master = true;
      };
    };

    cacheNetworks = [
      "127.0.0.0/8"
      "10.64.1.0/24"
      "10.64.0.0/24"
    ];
    extraOptions = ''
      recursion yes;
    '';
    extraConfig = ''
      zone "consul.in.redalder.org" IN {
        type forward;
        forward only;
        forwarders { 10.64.1.201 port 8600; };
      };
    '';
  };

  networking.firewall.allowedTCPPorts = [
    53
  ];
  networking.firewall.allowedUDPPorts = [
    53
  ];
}
