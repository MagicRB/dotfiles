inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    ({ lib, pkgs, config, secret, ... }:
      let
        HUPToConsul = pkgs.writeShellScriptBin "hup-to-consul"
          ''
            export PATH=${pkgs.procps}/bin:$PATH

            pkill -x -HUP consul
          '';
      in
        {
          magic_rb = {
            pins = inputs;
            overlays = inputs.self.overlays;

            hardware.toothpick = true;
            flakes.enable = true;
            sshdEmacs.enable = true;
            vpsRemoteAccess =
              { enable = true;
                trustedWheel = true;
              };
          };
          boot.loader.grub = {
            enable = true;
            version = 2;
            efiSupport = false;
          };

          environment.systemPackages =
            [ pkgs.git
              pkgs.envoy
            ];

          boot.kernel.sysctl = { "net.ipv4.ip_forward" = "1"; };

          services.nomad = {
            enable = true;
            enableDocker = false;
            dropPrivileges = false;

            extraPackages = with pkgs; [ consul glibc ];

            extraSettingsPaths = [ "/var/secrets/nomad.hcl" ];
          };

          # https://github.com/NixOS/nixpkgs/issues/76671
          # the rpc.statd daemon is not running when not mounting any nfs filesystems on boot
          # and can't be manually started...
          services.nfs.server.enable = true;

          # create default network with `podman -r network create default`
          # virtualisation.podman = {
          #  enable = true;
          # };

          virtualisation.docker = {
            enable = true;
          };

          services.consul = {
            enable = true;
            interface = {
              bind = "wg0";
              advertise = "wg0";
            };

            extraConfigFiles = [
              "/var/secrets/consul.hcl"
            ];
          };

          systemd.services.vault-agent = {
            restartTriggers = [ "/etc/vault-agent/vault.crt" "/etc/vault-agent/vault.key" ];
            serviceConfig = {
              ExecPreStart = "mkdir -p /var/secrets/ && chown -R vault-agent:secrets /var/secrets/";
            };
          };

          security.sudo.extraRules =
            [ { users = [ "vault-agent" ];
                runAs = "consul";
                commands =
                  [ { command = "${HUPToConsul}/bin/hup-to-consul";
                      options = [ "NOPASSWD" ];
                    }
                  ];
              }
            ];

          services.vault-agent = {
            enable = true;
            settings = {
              vault = {
                address = "https://${secret.network.ips.vault.dns}:8200";

                client_cert = "/etc/vault-agent/vault.crt";
                client_key = "/etc/vault-agent/vault.key";
              };

              auto_auth = {
                method = [
                  {
                    "cert" = {
                      name = "system-toothpick";
                    };
                  }
                ];

                sink = [
                  {
                    "file" = {
                      type = "file";
                      config = {
                        path = "/etc/vault-agent/vault.token";
                      };
                    };
                  }
                ];
              };

              template =
                let
                  refreshConsul = pkgs.writeShellScript "refresh-consul.sh" ''
                    export PATH=/run/wrappers/bin:$PATH

                    sudo -u consul ${HUPToConsul}/bin/hup-to-consul
                  '';
                in [
                  {
                    source = pkgs.writeText "vault.key.tpl" ''
                      {{ with secret "pki_dynra/issue/vault.toothpick" "common_name=vault.toothpick.dyn.redalder.org" "ttl=24h" "alt_names=localhost" "ip_sans=127.0.0.1" }}
                      {{ .Data.private_key }}
                      {{ end }}
                    '';
                    destination = "/etc/vault-agent/vault.key.new";
                  }
                  # create role with
                  #   vault write pki_dynra/roles/vault.toothpick \
                  #     allowed_domains=vault.toothpick.dyn.redalder.org \
                  #     allow_subdomains=false \
                  #     max_ttl=72h \
                  #     allow_bare_domains=true
                  #
                  # also requires bootstrapping by generating the key/crt
                  # the first time, look into the source template. Then the
                  # generated cert must once again be first added to the
                  # cert auth method, look into the command. Basically you
                  # must manually do what vault-agent will do on its own
                  # after bootstrap
                  {
                    source = pkgs.writeText "vault.crt.tpl" ''
                      {{ with secret "pki_dynra/issue/vault.toothpick" "common_name=vault.toothpick.dyn.redalder.org" "ttl=24h" "alt_names=localhost" "ip_sans=127.0.0.1" }}
                      {{ .Data.certificate }}
                      {{ end }}
                    '';
                    destination = "/etc/vault-agent/vault.crt.new";
                    command = pkgs.writeShellScript "vault.crt-renew" ''
                      export PATH=${pkgs.vault}/bin:${pkgs.procps}/bin:$PATH

                      set -ve

                      export VAULT_ADDR="https://${secret.network.ips.vault.dns}:8200/"
                      export VAULT_TOKEN="$(cat /etc/vault-agent/vault.token)"

                      vault write auth/cert/certs/vault.toothpick \
                        display_name=vault.toothpick \
                        policies=vault.toothpick \
                        certificate=@/etc/vault-agent/vault.crt.new \
                        ttl=24h

                      echo "$VAULT_TOKEN" > /var/secrets/vault.token

                      cp /etc/vault-agent/vault.crt.new /etc/vault-agent/vault.crt
                      cp /etc/vault-agent/vault.key.new /etc/vault-agent/vault.key

                      pkill sleep || true
                      ( sleep "$((12 * 60 * 60))" && pkill -HUP vault ) &
                      disown
                    '';
                  }
                  # create role with
                  #   vault write pki_dynra/roles/consul.toothpick \
                  #     allowed_domains=consul.toothpick.dyn.redalder.org \
                  #     allow_subdomains=false \
                  #     max_ttl=72h \
                  #     allow_bare_domains=true
                  #
                  {
                    source = pkgs.writeText "consul.crt.tpl" ''
                      {{ with secret "pki_dynra/issue/consul.toothpick" "common_name=consul.toothpick.dyn.redalder.org" "ttl=24h" "alt_names=localhost" "ip_sans=127.0.0.1" }}
                      {{ .Data.certificate }}
                      {{ end }}
                    '';
                    destination = "/var/secrets/consul.crt";
                    command = refreshConsul;
                  }
                  {
                    source = pkgs.writeText "consul.key.tpl" ''
                      {{ with secret "pki_dynra/issue/consul.toothpick" "common_name=consul.toothpick.dyn.redalder.org" "ttl=24h" "alt_names=localhost" "ip_sans=127.0.0.1" }}
                      {{ .Data.private_key }}
                      {{ end }}
                    '';
                    destination = "/var/secrets/consul.key";
                    command = refreshConsul;
                  }
                  {
                    source = pkgs.writeText "ca.crt.tpl" ''
                      {{ with secret "pki_dynra/cert/ca" }}
                      {{ .Data.certificate }}
                      {{ end }}
                    '';
                    destination = "/var/secrets/ca.crt";
                  }
                  {
                    source = pkgs.writeText "nomad.hcl.tpl" ''
                      client {
                        enabled = true

                        cni_path = "${pkgs.cni-plugins}/bin"

                        host_network "public" {
                          cidr = "64.225.104.221/32"
                          reserved_ports = ""
                        }

                        host_network "vpn" {
                          cidr = "${secret.network.networks.vpn}"
                          reserved_ports = ""
                        }
                      }

                      advertise {
                        http = "${secret.network.ips.toothpick}"
                        rpc  = "${secret.network.ips.toothpick}"
                        serf = "${secret.network.ips.toothpick}"
                      }

                      plugin "docker" {
                        config {
                          endpoint = "unix:///var/run/docker.sock"

                          allow_privileged = true
                        }
                      }

                      vault {
                        enabled = true
                        address = "https://${secret.network.ips.vault.dns}:8200"
                        allow_unauthenticated = false
                        create_from_role = "nomad-cluster"
                      }

                      {{ with secret "kv/data/systems/toothpick/nomad" }}
                      consul {
                        ssl = false
                        address = "127.0.0.1:8500"
                        # ca_file = "/var/secrets/ca.crt"
                        # key_file = "/var/secrets/consul.key"
                        # cert_file = "/var/secrets/consul.crt"

                        token = "{{ .Data.data.consul_token }}"
                        auto_advertise = true
                        server_auto_join = true
                        client_auto_join = true
                      }
                      {{ end }}

                      log_level = "DEBUG"
                      disable_update_check = true
                      datacenter = "do-1"
                      data_dir = "/var/lib/nomad"
                    '';
                    destination = "/var/secrets/nomad.hcl";
                    perms = "0644";
                  }
                  {
                    source = pkgs.writeText "consul.hcl.tpl" ''
                      datacenter = "do-1"
                      node_name = "toothpick"
                      data_dir = "/var/lib/consul"

                      retry_join_wan = [ "${secret.network.ips.blowhole.ip}" ]

                      server = true

                      primary_datacenter = "homelab-1"

                      acl {
                        enabled = true
                        default_policy = "deny"
                        enable_token_persistence = true
                        enable_token_replication = true

                        {{ with secret "kv/data/systems/toothpick/consul" }}
                        tokens {
                          "agent" = "{{ .Data.data.agent_token }}"
                          "replication" = "{{ .Data.data.replication_token }}"
                        }
                        {{ end }}
                      }

                      ui_config {
                        enabled = true
                      }

                      connect {
                        enabled = true
                      }

                      # ca_file = "/var/secrets/ca.crt"
                      # cert_file = "/var/secrets/consul.crt"
                      # key_file = "/var/secrets/consul.key"
                      verify_incoming = false
                      verify_outgoing = false
                      verify_server_hostname = false
                      log_level = "DEBUG"
                      ports {
                        http = 8500
                      #  https = 8501
                        grpc = 8502
                      }
                    '';

                    # ca_provider = "vault"
                    # ca_config {
                    #   address = "https://${secret.network.ips.vault.dns}:8200"
                    #   token = "{{ file "/var/secrets/vault.token" | trimSpace }}"
                    #   root_pki_path = "consul_root"
                    #   intermediate_pki_path = "consul_intermediate"
                    # }
                    destination = "/var/secrets/consul.hcl";
                    perms = "0644";
                  }
                ];
            };
          };

          networking = {
            hostName = "toothpick";

            nameservers =
              [ "${secret.network.ips.woodchip}"
                "93.184.77.2"
                "67.207.67.3"
              ];

            wireguard = {
              enable = true;
              interfaces."wg0" =
                {
                  postSetup = ''
                    ${pkgs.iptables}/bin/iptables -I FORWARD -i wg0 -o wg0 -j ACCEPT
                  '';

                  postShutdown = ''
                    ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -o wg0 -j ACCEPT
                  '';
                } // config.magic_rb.secret.wireguard."toothpick";


              interfaces."wg1" =
                {
                  postSetup = ''
                  '';

                  postShutdown = ''
                  '';
                } // config.magic_rb.secret.wireguard."toothpick-matej";
            };

            defaultGateway = "64.225.96.1";
            defaultGateway6 = "";
            dhcpcd.enable = false;
            usePredictableInterfaceNames = lib.mkForce false;

            firewall = {
              extraCommands = ''
            iptables -P FORWARD DROP
          '';

              # extraStopCommands = ''
              # '';

              interfaces."eth0" = {
                allowedTCPPorts =
                  [ 80
                    443
                  ];
                allowedUDPPorts =
                  [ 6666
                    6668
                  ];
              };

              interfaces."wg0" = {
                allowedTCPPorts =
                  [ 8501
                    8502
                    8301
                    8302
                    8300
                    10000
                  ];
                allowedTCPPortRanges =
                  [ {
                    from = 21000;
                    to = 21255;
                  }];
                allowedUDPPorts =
                  [ 8301
                    8302
                  ];
                allowedUDPPortRanges =
                  [ {
                    from = 21000;
                    to = 21255;
                  }];
              };
            };

            interfaces = {
              eth0 = {
                ipv4.addresses =
                  [ { address="64.225.104.221"; prefixLength=20; }
                    { address="10.19.0.6"; prefixLength=16; }
                  ];
                ipv6.addresses =
                  [ { address="fe80::8ce0:84ff:fefb:f981";
                      prefixLength=64;
                    }
                  ];
                ipv4.routes =
                  [ { address = "64.225.96.1"; prefixLength = 32; }
                  ];
              };
            };
          };

          security.pki.certificates =
            [ (builtins.readFile ../redalder.org.crt)
              (builtins.readFile ../dyn.redalder.org.crt)
            ];

          services.udev.extraRules = ''
        ATTR{address}=="8e:e0:84:fb:f9:81", NAME="eth0"
      '';

          time.timeZone = "Europe/Bratislava";
          system.stateVersion = "21.05";
        })
  ];
}
