{lib, config, pkgs, secret, ...}:
with lib;
let
in
{
  services.hashicorp.vault = {
    enable = true;

    package = pkgs.vault-bin;

    settings = {
      backend."file" = {
        path = "/var/lib/vault";
      };

      ui = true;

      listener = [
        {
          "tcp" = {
            address = "localhost:8200";
            tls_cert_file =
              "/var/secrets/${secret.network.ips.vault.dns}.crt.pem";
            tls_key_file =
              "/var/secrets/${secret.network.ips.vault.dns}.key.pem";
          };
        }
        {
          "tcp" = {
            address = "${secret.network.ips.blowhole.ip}:8200";
            tls_cert_file =
              "/var/secrets/${secret.network.ips.vault.dns}.crt.pem";
            tls_key_file =
              "/var/secrets/${secret.network.ips.vault.dns}.key.pem";
          };
        }
      ];

      storage."raft" = {
        path = "/var/lib/vault";
        node_id = "blowhole";
      };
      cluster_addr = "https://${secret.network.ips.blowhole.ip}:8201";
      api_addr = "http://${secret.network.ips.blowhole.ip}:8200";
    };
  };
}
