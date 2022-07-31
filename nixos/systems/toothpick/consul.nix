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
      datacenter = "do-1";
      data_dir = "/var/lib/consul";

      retry_join_wan = [ "${secret.network.ips.blowhole.ip}" ];

      server = true;

      bind_addr = secret.network.ips.toothpick;
      client_addr = secret.network.ips.toothpick;

      primary_datacenter = "homelab-1";

      acl = {
        enabled = true;
        default_policy = "deny";
        enable_token_persistence = true;
        enable_token_replication = true;
      };

      ports = {
        http = 8500;
        grpc = 8502;
      };

      ui_config.enabled = true;

      connect.enabled = true;

      # ca_file = "/var/secrets/consul-ca.crt";
      # cert_file = ""
      # key_file = ""
      verify_incoming = false;
      verify_outgoing = false;
      verify_server_hostname = false;
    };
  };

  systemd.services.hashicorp-consul.serviceConfig = {
    LimitNOFILE = mkForce "infinity";
    LimitNPROC = mkForce "infinity";
  };
}
