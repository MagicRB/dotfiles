{ config, pkgs, lib, ... }:
with lib;
let
  format = pkgs.formats.json { };

  hashiServiceModule =
    { config, ... }:
    let
      cfg' = config;
    in
    {
      options = {
        enable = mkEnableOption "Enable HashiCorp service";

        package = mkOption {
          type = with types;
            package;
        };

        settings = mkOption {
          type = format.type;
          default = {};
        };

        settingsFile = mkOption {
          type = with types;
            path;
          default = format.generate "${cfg'.package.pname}.json" cfg'.settings;
        };

        command = mkOption {
          type = with types;
            str;
          default =
            let
              switch =
                { "nomad" = "agent";
                  "vault" = "server";
                  "vault-bin" = "server";
                  "consul" = "agent";
                };
            in switch.${cfg'.package.pname} or "";
        };

        extraSettingsPaths = mkOption {
          type = with types;
            listOf path;
          default = [];
        };

        extraPluginPaths = mkOption {
          type = with types;
            listOf path;
          default = [];
        };

        extraArguments = mkOption {
          type = with types;
            listOf str;
          default = [];
        };

        extraPackages = mkOption {
          type = with types;
            listOf package;
          default = with pkgs;
            let
              switch =
                { "nomad" = [ coreutils iproute2 iptables ];
                  "vault" = [ ];
                  "vault-bin" = [ ];
                  "consul" = [ ];
                };
            in
              switch.${cfg'.package.pname} or [];
        };

        dynamic = mkOption {
          type = with types;
            nullOr package;
          default = null;
        };
      };
    };

  cfg = config.services.hashicorp;
in
{
  options.services.hashicorp = mkOption {
    type = with types;
      attrsOf (submodule hashiServiceModule);
    default = {};
  };

  config.environment.etc = flip mapAttrs' (filterAttrs (_: v: v.enable) cfg)
    (name: value:
      nameValuePair
        "${name}.d/main.json"
        { source = value.settingsFile; }
    );

  config.systemd.services = zipAttrsWith (const head)
    [ (flip mapAttrs' (filterAttrs (_: v: v.enable) cfg)
      (name: value:
        let
          configOpt =
            let
              switch =
                { "nomad" = "--config";
                  "consul" =  "--config-file";
                  "vault" = "--config";
                  "vault-bin" = "--config";
                };
            in
              switch.${value.package.pname} or "";
        in
          nameValuePair
            ("hashicorp-" + name)
            { description = name;

              wantedBy = [ "multi-user.target" ];
              wants = [ "network-online.target" ];
              after = [ "network-online.target" ];

              path = value.extraPackages;

              serviceConfig =
                { ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
                  ExecStart = "${value.package}/bin/${value.package.meta.mainProgram or value.package.pname} ${value.command} " +
                              (optionalString (value.package.pname != "vault" || value.command != "agent") "${configOpt}=/etc/${name}.d ") +
                              "${concatMapStringsSep " " (v: "${configOpt}=${v}") value.extraSettingsPaths} " +
                              "${concatMapStringsSep " " (v: "--plugin-dir=${v}/bin") value.extraPluginPaths} " +
                              (optionalString (value.package.pname == "vault" && value.command == "agent") "${configOpt}=/etc/${name}.d/main.json ") +
                              "${concatStringsSep " " value.extraArguments} ";

                  KillMode = "process";
                  KillSignal = "SIGINT";
                  LimitNOFILE = 65536;
                  LimitNPROC = "infinity";
                  OOMScoreAdjust = -1000;
                  Restart = "always";
                  RestartSec = 2;
                  TasksMax = "infinity";

                  StateDirectory = value.package.pname;
                };
            }
      ))
      (flip mapAttrs' (filterAttrs (_: v: v.enable && v.dynamic != null) cfg)
      (name: value:
        nameValuePair
          ("hashicorp-${name}-dynamic")
          { description = name;

            wantedBy = [ "hashicorp-${name}.service" ];
            wants = [ "network-online.target" ];
            after = [ "network-online.target" ];
            before = [ "hashicorp-${name}.service" ];

            path = value.extraPackages;

            restartIfChanged = true;

            serviceConfig =
              { ExecStart = value.dynamic;
                RemainAfterExit = true;
                Type = "oneshot";
              };
          }
      ))
    ];
}
