{ hostName }:
inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    ({ pkgs, config, ... }:
      {
        magic_rb = {
          grub = {
            enable = true;
            efi.enable = true;
            devices = [ "nodev" ];
          };

          pins = inputs;
          overlays = inputs.self.overlays;

          hardware.${hostName} = true;
          flakes.enable = true;
          sshdEmacs.enable = true;
          vpsRemoteAccess =
            { enable = true;
              trustedWheel = true;
            };
        };

        users.groups.nix-cache =
          { gid = 1500; };
        users.users.nix-cache =
          { shell = "${pkgs.coreutils}/bin/nologin";
            group = "nix-cache";
            isSystemUser = true;
            home = "/var/nix-cache";
            description = "User for uploading things to the cache.";

            uid = 1500;
            openssh.authorizedKeys.keys =
              [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFVkFvalffJ/SMjJGG3WPiqCqFygnWzhGUaeALBIoCsJ (none)" ];
          };

        environment.systemPackages =
          [ pkgs.git ];

        services.openssh = {
          extraConfig = ''
            Match User nix-cache
              ChrootDirectory /var/nix-cache
              ForceCommand internal-sftp -d /cache
              AllowTcpForwarding no
          '';
        };

        services.nginx = {
          enable = true;

          config = (inputs.nixng.lib "${pkgs.stdenv.system}").generators.toNginx
            [
              {
                worker_processes = 2;

                events."" = {
                  use = "epoll";
                  worker_connections = 128;
                };

                http."" = {
                  server_tokens = "off";

                  include = [
                    [ "${pkgs.nginx}/conf/mime.types" ]
                  ];
                  charset = "utf-8";
                  access_log = "off";

                  server."" = {
                    listen = [ "80" "default_server" ];
                    server_name = [
                      "${hostName}.redalder.org"
                    ];

                    location."/" = {
                      return = "404";
                    };

                    location."/cache/" = {
                      root = "/var/nix-cache";
                    };
                  };
                };
              }
            ];
        };

        networking = {
          firewall = {
            allowedTCPPorts = [ 22 80 ];
          };

          interfaces.ens3.useDHCP = true;

          firewall.enable = true;
          inherit hostName;
        };

        time.timeZone = "Europe/Bratislava";
        system.stateVersion = "20.09";
        security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];
      })
  ];

}
