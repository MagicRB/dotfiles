inputs: {
  system = "x86_64-linux";
  hostname = "heater";
  check = false;

  config = {
    allowUnfree = true;
  };

  hm."main" = import ../home-manager/profiles/common.nix {
    multimc5 = true;
    wine = true;
    _3dPrinting = true;
    js-ts = true;
  };

  modules = [
    ../nixos/hardware/heater.nix # auto
    ../nixos/modules/efi-grub.nix # manual
    ../nixos/profiles/workstation.nix # auto
    ../nixos/modules/pin-nixpkgs.nix # manual
    ../nixos/users/main.nix # auto
    ../nixos/modules/nomad.nix # manual
    ../nixos/modules/xserver.nix # manual
    ../nixos/modules/vault-agent.nix # manual
    ../nixos/modules/erase-my-darlings.nix # manual
    ({ nixpkgs, nixpkgs-unstable, ... }: _: {
      magic_rb = {
        grub = {
          enable = true;
          efi.enable = true;
        };

        xserver = {
          enable = true;
          gpu = "nvidia";
          xmonad = true;

          setSkLayout = true;
          emacsCtrl = true;
        };

        pins = {
          "nixpkgs" = inputs.nixpkgs;
          "nixpkgs-unstable" = inputs.nixpkgs-unstable;
          "nixpkgs-master" = inputs.nixpkgs-master;
        };

        erase-my-darlings = {
          enable = true;
          snapshot = "heater-zpool/local/root@blank";
        };
      };

      nixpkgs.pkgs = nixpkgs;

      hardware.steam-hardware.enable = true;
    
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
                  name = "system-heater";
                };
              }
            ];
          };

          template = [
            {
              source = nixpkgs.writeText "wg0.key.tpl" ''
                {{ with secret "kv/data/systems/heater/wireguard" }}{{ .Data.data.private_key }}{{ end }}
              '';
              destination = "/var/secrets/wg0.key";
            }
            {
              source = nixpkgs.writeText "nomad.hcl.tpl" ''
                client {
                  enabled = true 
                 servers = [ "blowhole.in.redalder.org:4647" ]

                 options {
                    docker.privileged.enabled = "true"
                  }
                  
                  cni_path = "${nixpkgs.cni-plugins}/bin"
                } 
                 {{ with secret "kv/data/systems/heater/nomad" }}
                vault {
                  enabled = true
                  address = "https://vault.in.redalder.org:8200"
                  token = "{{ .Data.data.vault_token }}"
                  allow_unauthenticated = true
                 create_from_role = "nomad-cluster"
               }

               consul {
                 address = "blowhole.in.redalder.org:8500"
                 token = "{{ .Data.data.consul_token }}"
               }
               {{ end }}

               disable_update_check = true
               datacenter = "homelab-1"
               data_dir = "/var/lib/nomad"
             '';
             destination = "/var/secrets/nomad.hcl";
             perms = "0644";
           }
         ];
       };
     };

     services.nomad = {
       enable = true;
       enableDocker = true;
       dropPrivileges = false;

       extraPackages = [ nixpkgs.consul ];

       package = nixpkgs-unstable.nomad;

       extraSettingsPaths = [ "/var/secrets/nomad.hcl" ];
     };

     services.hydra = {
       enable = true;
       hydraURL = "http://localhost:3000";
       notificationSender = "hydra@localhost"; # e-mail of hydra service
       # a standalone hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
       buildMachinesFiles = [];
       # you will probably also want, otherwise *everything* will be built from scratch
       useSubstitutes = true;

       extraConfig = ''
         <runcommand>
           command = cp $HYDRA_JSON /tmp/hydra-json ; ${nixpkgs.curl}/bin/curl -X POST -H "Content-Type: application/json" -d @$HYDRA_JSON http://localhost:8080/hydra
         </runcommand>
       '';
     };

     nix.extraOptions = ''
       allowed-uris = https://gitea.redalder.org/RedAlder/rlib
     '';

     networking = {
       firewwall = {
         allowedUDPPorts = [ 6666 ];
       };

       wireguard.interfaces = {
         wg0 = {
           ips = [ "10.64.0.3/24" ];
           listenPort = 6666;

           privateKeyFile = "/var/secrets/wg0.key";
           peers = [
             {
               publicKey = "h4g6vWjOB6RS0NbrP/Kvb2CZeutm/F+ZfDbJmEd1Dgk=";
               allowedIPs = [ "10.64.0.0/24" ];
               endpoint = "redalder.org:6666";
               persistentKeepalive = 25;
             }
           ];
         };
       };
     };
   })
  ] ++ [
    (_: _: {
      networking = {
        hostName = "heater";
        useDHCP = false;
        interfaces.enp3s0.useDHCP = true;

        firewall.enable = false;
        hostId = "3457b383";
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];

      virtualisation.docker.enable = true;
      boot = {
        supportedFilesystems = [ "zfs" ];
        zfs.enableUnstable = true;
      };
    })
  ];

  compatModules = [];
}
