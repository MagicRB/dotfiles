{ pkgs, lib, config, tf, ... }:
with lib;
let
in
{
  services.hashicorp.vault-agent =
    { enable = true;
      package = pkgs.vault;

      command = "agent";

      extraPackages = with pkgs;
        [ sudo getent ];

      settings =
        { vault =
            { address = "https://localhost:8200";
              retry =
                { num_retries = 5;
                };
            };

          auto_auth.method = singleton
            { "approle" =
                { mount_path = "auth/approle";
                  config =
                    { role_id_file_path = "/var/secrets/approle.roleid";
                      secret_id_file_path = "/var/secrets/approle.secretid";
                      remove_secret_id_file_after_reading = false;
                    };
                };
            };

          sink =
            [ { type = "file";
                config =
                { path = "/var/secrets/vault-token";
                };
              }
            ];

          template = [
            { source = pkgs.writeText "consul.json.vtmpl" ''
                {
                  "encrypt": "{{ with secret "kv/data/homelab-1/blowhole/consul/encryption_key" }}{{ or .Data.data.key "" }}{{ end }}",
                  "acl": {
                    "tokens": {
                      "agent": "{{ with secret "kv/data/homelab-1/blowhole/consul/agent_token" }}{{ or .Data.data.secret "" }}{{ end }}",
                      "default": "{{ with secret "kv/data/homelab-1/blowhole/consul/anonymous_token" }}{{ or .Data.data.secret "" }}{{ end }}"
                    }
                  }
                }
              '';
              destination = "/run/secrets/consul.json";
              command = pkgs.writeShellScript "consul-command"
                ''
                  sudo systemctl try-restart-or-reload hashicorp-consul.service
                '';
            }
            { source = pkgs.writeText "nomad.json.vtmpl" ''
                {
                  "server": {
                    "encrypt": "{{ with secret "kv/data/homelab-1/blowhole/nomad/encryption_key" }}{{ or .Data.data.key "" }}{{ end }}"
                  },
                  "vault": {
                    "token": "{{ with secret "kv/data/homelab-1/blowhole/nomad/vault_token" }}{{ or .Data.data.secret "" }}{{ end }}"
                  },
                  "consul": {
                    "token": "{{ with secret "kv/data/homelab-1/blowhole/nomad/consul_token" }}{{ or .Data.data.secret "" }}{{ end }}"
                  }
                }
              '';
              destination = "/run/secrets/nomad.json";
              command = pkgs.writeShellScript "nomad-command"
                ''
                  sudo systemctl try-restart-or-reload hashicorp-nomad.service
                '';
            }
          ];
        };
    };
}
