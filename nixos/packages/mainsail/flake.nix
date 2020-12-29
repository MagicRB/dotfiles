{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {
      nixosModules.mainsail = { pkgs, config, ... }: with nixpkgs.lib;
        let
          cfg = config.services.mainsail;
        in {
          options.services.mainsail = {
            enable = mkOption {
              description = "Enable Mainsail NOTE: enable nginx too";
              default = false;
              type = types.bool;
            };

            apiServer = mkOption {
              description = "IP and port of the moonraker apiserver";
              default = "localhost:7125";
              type = types.str;
            };

            printerCam = mkOption {
              description = "IP and port of the webcam server";
              default = "";
              type = types.str;
            };

            virtualHost = mkOption {
              description = "The virtual host for mainsail in nginx";
              default = "mainsail.localhost";
              type = types.str;
            };
          };

          config = mkIf cfg.enable {
            services.nginx = {
              enable = mkForce true;

              recommendedGzipSettings = true;
              recommendedOptimisation = true;

              clientMaxBodySize = "200M";

              # commonHttpConfig = ''
              #   map $http_upgrade $connection_upgrade {
              #     default upgrade;
              #     \'\' close;
              #   } 
              # '';

              virtualHosts."${cfg.virtualHost}" = {
                root = pkgs.fetchzip {
                  url = "https://github.com/meteyou/mainsail/releases/download/v0.4.0/mainsail.zip";
                  sha256 = "PBu3ktVCchi+E54cuKzsRhZXWkWG/NvOf+IZNSNN8fw=";
                  stripRoot = false;
                };

                locations = let
                  apiServer = cfg.apiServer;
                  printerCam = cfg.printerCam;
                  extraConfig = ''
                    # proxy_set_header Host $host;
                    # proxy_set_header X-Real-IP $remote_addr;
                    # proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                    # proxy_set_header X-Scheme $scheme;
                  '';
                in {
                  "/" = {
                    extraConfig = ''
                      try_files $uri $uri/ /index.html;
                    '';
                  };

                  "/printer" = {
                    proxyPass = "http://${apiServer}/printer";
                    inherit extraConfig;
                  };

                  "/api" = {
                    proxyPass = "http://${apiServer}/api";
                    inherit extraConfig;
                  };

                  "/access" = {
                    proxyPass = "http://${apiServer}/access";
                    inherit extraConfig;
                  };

                  "/websocket" = {
                    proxyPass = "http://${apiServer}/websocket";
                    extraConfig = ''
                      proxy_http_version 1.1;
                      proxy_set_header Upgrade $http_upgrade;
                      proxy_set_header Connection $connection_upgrade;
                      proxy_set_header Host $host;
                      proxy_set_header X-Real-IP $remote_addr;
                      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                      proxy_read_timeout 86400;
                    '';
                  };

                  "/machine" = {
                    proxyPass = "http://${apiServer}/machine";
                    inherit extraConfig;
                  };

                  "/server" = {
                    proxyPass = "http://${apiServer}/server";
                    inherit extraConfig;
                  };

                  "/webcam/" = mkIf (printerCam != "") {
                    proxyPass = "http://${printerCam}/";
                  };
                };
              };
            };
          };
        };
    };
}
