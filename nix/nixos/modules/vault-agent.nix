{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
with lib;
let
  cfg = config.services.vault-agent;
  format = nixpkgs-unstable.formats.json { };
in
{
  options = {
    services.vault-agent = {
      enable = mkEnableOption "Vault, secure credentials storage and manager";

      package = mkOption {
        type = types.package;
        default = nixpkgs-unstable.vault;
        defaultText = "nixpkgs.vault";
        description = ''
          The package used for the Vault agent and CLI.
        '';
      };

      secretsDir = mkOption {
        type = types.nullOr types.path;
        default = "/var/secrets";
        description = ''
          Vault secrets directory;
        '';
      };

      settings = mkOption {
        type = format.type;
        default = {};
        description = ''
          Configuration for Vault Agent. See the <link xlink:href="https://www.vaultproject.io/docs/agent#configuration">documentation</link>
        '';
      };

      secretsGroup = mkOption {
        type = types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              default = "secrets";
              description = ''
                Group used for accessing the secrets.
              '';
            };
            id = mkOption {
              type = types.int;
              default = 1984;
              description = ''
                Group ID for the secrets group. 
              '';
            };
          };
        };
        default = {};
      };

      userName = mkOption {
        type = types.str;
        default = "vault-agent";
      };

      groupName = mkOption {
        type = types.str;
        default = "vault-agent";
      };

      uid = mkOption {
        type = types.int;
        default = 1985;
      };

      gid = mkOption {
        type = types.int;
        default = 1985;
      };
    };
  };

  config = mkIf cfg.enable
    (let
      vaultConfig = format.generate "vault.json" cfg.settings;
    in
      {
        users = {
          users = {
            "${cfg.userName}" = {
              group = cfg.groupName;
              uid = cfg.uid;
            };
          };
          groups = {
            "${cfg.groupName}" = {
              gid = cfg.gid;
            };

            "${cfg.secretsGroup.name}" = {
              gid = cfg.secretsGroup.id;
            };
          };
        };

        systemd.tmpfiles.rules = mkIf (cfg.secretsDir != null) [
          "d ${cfg.secretsDir} 6755 vault-agent ${cfg.secretsGroup.name} 0"
        ];

        systemd.services.vault-agent = {
          description = "Vault Agent";

          wantedBy = [ "multi-user.target" ];
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];

          path = (with nixpkgs-unstable; [
            glibc
          ]);

          serviceConfig = mkMerge [
            {
              User = cfg.userName;
              Group = cfg.groupName;

              ExecReload = "${nixpkgs-unstable.busybox}/bin/kill -HUP $MAINPID";
              ExecStart = "${cfg.package}/bin/vault agent -config=${vaultConfig}";

              KillMode = "process";
              KillSignal = "SIGINT";
              LimitNOFILE = 65536;
              LimitNPROC = "infinity";
              OOMScoreAdjust = -1000;
              Restart = "on-failure";
              RestartSec = 2;
              TasksMax = "infinity";

              ConfigurationDirectory = "vault-agent";
              ConfigurationDirectoryMode = "0700";
            }
          ];
        };
      });
}
