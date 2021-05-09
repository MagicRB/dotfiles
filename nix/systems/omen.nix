inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    inputs.home-manager.nixosModules.home-manager
    ({ pkgs, config, ... }: {
      home-manager.users."main" =
        { ... }: {
          imports = [ ../home-manager/modules/default.nix ];

          magic_rb = {
            pins = {
              inherit (inputs)
                nixpkgs
                nixpkgs-unstable
                nixpkgs-master

                home-manager
                nixng
                fenix;
            };
            config = {
              allowUnfree = true;
            };
            overlays = inputs.self.overlays;

            programs = {
              alacritty.enable = true;
              bash = {
                enable = true;
                enableDirenv = true;
              };
              ssh.enable = true;
              emacs.enable = true;
              xmonad.enable = true;
              gpg.enable = true;
              multimc.enable = false;
            };
            packageCollections = {
              "3dPrinting".enable = false;
              cmdline.enable = true;
              graphical.enable = true;
              rust.enable = true;
              webdev.enable = false;
              wine.enable = false;
            };
          };

          home.stateVersion = "20.09";
        };

      services.vault-agent = {
        enable = true;
        settings = {
          vault = {
            address = "https://vault.in.redalder.org:8200";

            client_cert = "/etc/vault-agent/client.crt";
            client_key = "/etc/vault-agent/client.key";
          };

          auto_auth = {
            method = [
              {
                "cert" = {
                  name = "system-omen";
                };
              }
            ];
          };

          template = [
            {
              source = pkgs.writeText "wg0.key.tpl" ''
                {{ with secret "kv/data/systems/omen/wireguard" }}{{ .Data.data.private_key }}{{ end }}
              '';
              destination = "/var/secrets/wg0.key";
            }
          ];
        };
      };

      magic_rb = {
        grub = {
          enable = true;
          efi.enable = true;
        };

        xserver = {
          enable = true;
          gpu = "nvidia";
          xmonad = true;

          nvidia = {
            prime = true;

            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          };

          setSkLayout = true;
          emacsCtrl = true;
        };

        pins = {
          inherit (inputs)
            nixpkgs
            nixpkgs-unstable
            nixpkgs-master

            home-manager
            nixng
            fenix;
        };
        config = {
          allowUnfree = true;
        };
        overlays = inputs.self.overlays;

        hardware.omen = true;
        flakes.enable = true;
        pulseaudio.enable = true;
        sshdEmacs.enable = true;
        networking = {
          bluetooth = true;
          networkManager = true;
        };
      };

      programs.steam.enable = true;

      services.sshd.enable = true;

      networking = {
        hostName = "omen";
        useDHCP = false;
        interfaces.eno1.useDHCP = true;
        hostId = "10c7ffc5";

        wireguard.interfaces = {
          wg0 = {
            ips = [ "10.64.0.8/24" ];
            listenPort = 6666;

            privateKeyFile = "/var/secrets/wg0.key";
            peers = [
              {
                publicKey = "h4g6vWjOB6RS0NbrP/Kvb2CZeutm/F+ZfDbJmEd1Dgk=";
                allowedIPs = [ "10.64.0.0/24" "10.64.1.0/24" ];
                endpoint = "redalder.org:6666";
                persistentKeepalive = 30;
              }
            ];
          };
        };
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";
    })
  ];
}
