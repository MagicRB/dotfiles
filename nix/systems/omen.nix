inputs: {
  system = "x86_64-linux";
  hostname = "omen";
  check = false;

  config = {
    allowUnfree = true;
  };

  hm."main" = import ../home-manager/profiles/common.nix {
    multimc5 = false;
    wine = false;
    _3dPrinting = false;
    js-ts = false;
  };

  modules = [
    ../nixos/hardware/omen.nix # auto
    ../nixos/modules/efi-grub.nix # manual
    ../nixos/modules/pin-nixpkgs.nix # manual
    ../nixos/users/main.nix # auto
    ../nixos/profiles/laptop.nix # auto
    ../nixos/modules/xserver.nix # manual
  ] ++ [
    ({ nixpkgs, ... }: _: {
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
              source = nixpkgs.writeText "wg0.key.tpl" ''
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
          "nixpkgs" = inputs.nixpkgs;
          "nixpkgs-unstable" = inputs.nixpkgs-unstable;
          "nixpkgs-master" = inputs.nixpkgs-master;
        };
      };

      nixpkgs.pkgs = nixpkgs;

      hardware.steam-hardware.enable = true;

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

  compatModules = [];
}
