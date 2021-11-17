inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    inputs.dwarffs.nixosModules.dwarffs
    inputs.home-manager.nixosModules.home-manager

    ({ pkgs, config, lib, secret, ... }:
      let
        inherit (config.magic_rb.pkgs) nixpkgs-unstable;
      in
        {
          home-manager.users."main" =
            { ... }:
            {
              imports = [ ../home-manager/modules/default.nix ];

              magic_rb = {
                optimisation.march = "znver2";
                pins = inputs;
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
                  multimc.enable = true;
                };
                packageCollections = {
                  "3dPrinting".enable = true;
                  cmdline.enable = true;
                  graphical.enable = true;
                  rust.enable = true;
                  webdev.enable = true;
                  wine.enable = true;
                };
              };

              services.syncthing.enable = true;

              home.stateVersion = "20.09";
            };

          magic_rb = {
            optimisation.march = "znver2";
            grub = {
              enable = true;
              efi.enable = true;
            };

            xserver = {
              enable = true;
              gpu = "nvidia";
              xmonad = true;

              qwertyNeo2 = true;
              mimickInTty = true;
            };

            pins = inputs;
            config = {
              allowUnfree = true;
            };
            overlays = inputs.self.overlays;

            erase-my-darlings.zfs = {
              enable = true;
              snapshot = "heater-zpool/local/root@blank";
            };

            hardware.heater = true;
            sshdEmacs.enable = true;
            flakes = {
              enable = true;
              nixMaster = true;
            };
            pulseaudio.enable = true;
          };

          programs.steam.enable = true;

          boot.binfmt.emulatedSystems = [
            "aarch64-linux"
          ];
          
          services.vault-agent = {
            enable = true;
            settings = {
              vault = {
                address = "https://${secret.network.ips.vault.vpn}:8200";

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
                  source = pkgs.writeText "wg0.key.tpl" ''
                    {{ with secret "kv/data/systems/heater/wireguard" }}{{ .Data.data.private_key }}{{ end }}
                  '';
                  destination = "/var/secrets/wg0.key";
                }
                {
                  source = pkgs.writeText "nomad.hcl.tpl" ''
                    client {
                      enabled = true 
                      servers = [ "${secret.network.ips.blowhole.dns}:4647" ]

                      options {
                        docker.privileged.enabled = "true"
                      }
                  
                      cni_path = "${nixpkgs-unstable.cni-plugins}/bin"
                    } 

                    {{ with secret "kv/data/systems/heater/nomad" }}
                    vault {
                      enabled = true
                      address = "https://${secret.network.ips.vault.vpn}:8200"
                      token = "{{ .Data.data.vault_token }}"
                      allow_unauthenticated = true
                      create_from_role = "nomad-cluster"
                    }

                    consul {
                      address = "${secret.network.ips.blowhole.dns}:8500"
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

          environment.systemPackages =
            (with pkgs;
              [ (steam.override
                { extraPkgs = pkgs: with pkgs; [ pango harfbuzz libthai ];
                  extraLibraries = pkgs: with config.hardware.opengl;
                    if pkgs.hostPlatform.is64bit
                    then [ package ] ++ extraPackages
                    else [ package32 ] ++ extraPackages32;
                })
              ]);

          environment.enableDebugInfo = true;

          services.postgresql = {
            package = pkgs.postgresql_13;
            enable = true;
          };

          services.openssh = {
            enable = true;
          };

          services.nomad = {
            enable = false; # Consul conflict, services go yeet
            enableDocker = false;
            dropPrivileges = false;

            extraPackages = [ nixpkgs-unstable.consul ];

            package = nixpkgs-unstable.nomad;

            extraSettingsPaths = [ "/var/secrets/nomad.hcl" ];
          };

          networking = {
            firewall = {
              allowedTCPPorts = [ 22 25565 ];
            };
          };

          ## Fuck podman, 2021-08-31, `podman info` fails with a stack trace
          ## and all containers tested fail with `Operation not supported`...
          # virtualisation.podman = {
          #   enable = true;
          #   dockerCompat = true;
          # };

          virtualisation.docker.enable = true;

          hardware.firmware = [ pkgs.firmwareLinuxNonfree ];
        })
  ] ++ [
    ({ pkgs, ... }: {
      networking = {
        hostName = "heater";
        useDHCP = false;
        interfaces.enp3s0.useDHCP = true;

        firewall.enable = true;
        hostId = "3457b383";
      };

      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];
    })
  ];
}
