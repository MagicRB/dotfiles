{inputs, lib, config, pkgs, secret, ...}:
with lib;
let
in
{
  services.hashicorp.consul = {
    enable = true;

    extraSettingsPaths =
      [ "/run/secrets/consul.json"
      ];
    package = pkgs.callPackage ("${inputs.nixpkgs-master}/pkgs/servers/consul/default.nix") {};

    settings = {
      datacenter = "homelab-1";
      data_dir = "/var/lib/consul";
      log_level = "DEBUG";

      server = true;

      bind_addr = secret.network.ips.blowhole.ip;
      client_addr = secret.network.ips.blowhole.ip;

      primary_datacenter = "homelab-1";

      acl = {
        enabled = true;
        default_policy = "deny";
        enable_token_persistence = true;
      };

      ports = {
        http = 8500;
        grpc = 8502;
      };

      connect = {
        enabled = true;
      };

      ca_file = "/var/secrets/consul-ca.crt";
      # cert_file = ""
      # key_file = ""
      verify_incoming = false;
      verify_outgoing = false;
      verify_server_hostname = false;

      ui_config.enabled = true;
      domain = "consul.in.redalder.org";
    };
  };

  systemd.services.hashicorp-consul.serviceConfig = {
    LimitNOFILE = mkForce "infinity";
    LimitNPROC = mkForce "infinity";
  };
}
