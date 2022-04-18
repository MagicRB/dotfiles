inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    inputs.home-manager.nixosModules.home-manager
    ({ pkgs, config, lib, secret, ... }:
      let
        inherit (config.magic_rb.pkgs) nixpkgs-unstable;
      in
        with lib;
        {
          home-manager.users."main" =
            { config, ... }: {
              imports = [ ../home-manager/modules/default.nix ];

              magic_rb = {
                pins = inputs;
                overlays = inputs.self.overlays;

                programs = {
                  bash.enable = true;
                };
              };

              home.stateVersion = "21.05";
            };

          magic_rb = {
            pins = inputs;
            overlays = inputs.self.overlays;

            grub = {
              enable = true;
              efi.enable = true;
            };

            hardware.blowhole = true;

            sshdEmacs.enable = true;
            flakes = {
              enable = true;
            };

            vpsRemoteAccess =
              { enable = true;
                trustedWheel = true;
              };
          };

          services.openssh = {
            enable = true;
          };

          services.nfs.server = {
            enable = true;
            lockdPort = 4001;
            mountdPort = 4002;
            statdPort = 4000;
          };

          systemd.services.consul.serviceConfig =
            { LimitNOFILE = "infinity";
              LimitNPROC = "infinity";
            };

          systemd.services.consul.preStart =
            let
              orig = pkgs.writeText "consul.json" (builtins.toJSON
                {
                  datacenter = "homelab-1";
                  data_dir = "/var/lib/consul";
                  encrypt = "%%consul-encrypt.key%%";
                  log_level = "DEBUG";

                  server = true;

                  bind_addr = secret.network.ips.blowhole.ip;
                  client_addr = secret.network.ips.blowhole.ip;

                  primary_datacenter = "homelab-1";

                  acl = {
                    enabled = true;
                    default_policy = "deny";
                    enable_token_persistence = true;
                    tokens = {
                      "agent" = "%%consul-agent.token%%";
                    };
                  };

                  ports = {
                    http = 8500;
                    grpc = 8502;
                  };

                  connect = {
                    enabled = true;
                  };

                  ca_file = "/var/secrets/consul-ca.crt";
                  # cert_file = ""
                  # key_file = ""
                  verify_incoming = false;
                  verify_outgoing = false;
                  verify_server_hostname = false;
                });
            in
              ''
                mkdir -p /run/cfg/consul/

                sed -e 's~%%consul-encrypt.key%%~'"$(cat /var/secrets/consul-encrypt.key)"'~' \
                    -e 's~%%consul-agent.token%%~'"$(cat /var/secrets/consul-agent.token)"'~' \
                  ${orig} > /run/cfg/consul/consul.json
              '';

          services.consul = {
            enable = true;

            extraConfigFiles = singleton "/run/cfg/consul/consul.json";
          };

          systemd.services.nomad.preStart =
            let
              orig = pkgs.writeText "nomad.json" (builtins.toJSON
                {
                  server = {
                    enabled = true;
                    bootstrap_expect = 1;

                    encrypt = "%%nomad-encrypt.key%%";
                  };

                  tls = {
                    # http = false # true
                    # rpc = true

                    # ca_file   = "nomad-ca.pem"
                    # cert_file = "client.pem"
                    # key_file  = "client-key.pem"

                    # verify_server_hostname = true
                    # verify_https_client    = true
                  };

                  vault = {
                    enabled = true;
                    address = "https://${secret.network.ips.vault.dns}:8200";
                    token = "%%nomad-vault.token%%";
                    allow_unauthenticated = true;
                    create_from_role = "nomad-cluster";
                  };

                  consul = {
                    address = "${secret.network.ips.blowhole.ip}:8500";
                    token = "%%nomad-consul.token%%";
                    auto_advertise = true;
                    server_auto_join = true;
                    client_auto_join = true;
                  };

                  acl = {
                    enabled = true;
                  };

                  client = {
                    cni_path = "${pkgs.cni-plugins}/bin";

                    options = {
                      "docker.privileged.enabled" = "true";
                    };

                    host_network."vpn" = {
                      cidr = secret.network.networks.vpn;
                    };

                    host_volume."renderD128" = {
                      path = "/dev/dri/renderD128";
                    };

                    host_volume."card0" = {
                      path = "/dev/dri/card0";
                    };

                    host_volume."jellyfin-mount" = {
                      path = "/mnt/jellyfin-mount";
                    };

                    host_volume."sonoff" = {
                      path = "/dev/serial/by-id/usb-ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_4c004e9c53c9eb118a9f8b4f1d69213e-if00-port0";
                    };

                    enabled = true;
                  };

                  plugin."docker" = {
                    config = {
                      allow_caps =
                        [ "CHOWN" "DAC_OVERRIDE" "FSETID" "FOWNER" "MKNOD"
                          "NET_RAW" "SETGID" "SETUID" "SETFCAP" "SETPCAP"
                          "NET_BIND_SERVICE" "SYS_CHROOT" "KILL" "AUDIT_WRITE"
                          "SYS_ADMIN"
                        ];
                      allow_privileged = true;
                    };
                  };

                  disable_update_check = true;
                  datacenter = "homelab-1";
                  data_dir = "/var/lib/nomad";
                });
            in
              ''
                mkdir -p /run/cfg/nomad/

                sed -e 's~%%nomad-encrypt.key%%~'"$(cat /var/secrets/nomad-encrypt.key)"'~' \
                    -e 's~%%nomad-consul.token%%~'"$(cat /var/secrets/nomad-consul.token)"'~' \
                    -e 's~%%nomad-vault.token%%~'"$(cat /var/secrets/nomad-vault.token)"'~' \
                  ${orig} > /run/cfg/nomad/nomad.json
              '';

          services.nomad = {
            enable = true;
            enableDocker = false;
            dropPrivileges = false;

            extraPackages = with pkgs; [ consul glibc ];
            extraSettingsPaths = singleton "/run/cfg/nomad/nomad.json";
          };

          virtualisation.docker = {
            enable = true;
          };

          systemd.tmpfiles.rules = singleton "d /run/cfg/vault 0750 vault vault 1d";

          systemd.services.vault =
            { serviceConfig.ExecStart = mkForce "${config.services.vault.package}/bin/vault server -config /run/cfg/vault/vault.json";
              preStart =
                let
                  orig = pkgs.writeText "vault.json" (builtins.toJSON
                    {
                      backend."file" = {
                        path = "/var/lib/vault";
                      };

                      listener = [
                        { "tcp" =
                            { address = "localhost:8200";
                              tls_cert_file = "/var/secrets/${secret.network.ips.vault.dns}.crt.pem";
                              tls_key_file = "/var/secrets/${secret.network.ips.vault.dns}.key.pem";
                            }; }
                        { "tcp" =
                            { address = "${secret.network.ips.blowhole.ip}:8200";
                              tls_cert_file = "/var/secrets/${secret.network.ips.vault.dns}.crt.pem";
                             tls_key_file = "/var/secrets/${secret.network.ips.vault.dns}.key.pem";
                           }; }
                      ];

                      storage."consul" = {
                        address = "${secret.network.ips.blowhole.ip}:8500";
                        cluster_addr  = "https://${secret.network.ips.blowhole.ip}:8201";
                        redirect_addr = "http://${secret.network.ips.blowhole.ip}:8200";
                        path = "vault/";
                        token = "%%vault-consul.token%%";
                      };
                    });
                in
                  ''
                    mkdir -p /run/cfg/vault/

                    sed -e 's~%%vault-consul.token%%~'"$(cat /var/secrets/vault-consul.token)"'~' \
                      ${orig} > /run/cfg/vault/vault.json
                  '';
            };

          services.vault = {
            enable = true;
          };

          environment.etc.exports.enable = false;

          services.moonraker = {
            enable = true;

            settings = {
              authorization = {
                trusted_clients = with secret.network.ips;
                  [ "127.0.0.1" heater edge.vpn omen.vpn
                  ];
              };

              octoprint_compat = {};
              history = {};
            };
          };

          services.nginx = {
            enable = true;

            recommendedGzipSettings = true;
            recommendedProxySettings = true;
            recommendedOptimisation = true;

            upstreams."apiserver" = {
              servers."127.0.0.1:7125" = {};
              extraConfig = ''
              ip_hash;
            '';
            };

            virtualHosts.${secret.network.ips.blowhole.dns} = {
              root = pkgs.magic_rb.mainsail;

              locations."/".extraConfig = ''
                try_files $uri $uri/ /index.html;
              '';

              locations."/index.html".extraConfig = ''
                add_header Cache-Control "no-store, no-cache, must-revalidate";
              '';

              locations."/websocket".extraConfig = ''
                proxy_pass http://apiserver/websocket;
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection $connection_upgrade;
                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_read_timeout 86400;
              '';

              locations."~ ^/(printer|api|access|machine|server)/".extraConfig = ''
                proxy_pass http://apiserver$request_uri;
                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Scheme $scheme;
              '';

              extraConfig = ''
                client_max_body_size 512M;
              '';
            };
          };

          users.users.klipper =  {
            home = "/var/lib/klipper";
            isSystemUser = true;
            group = "klipper";
            uid = 321;
          };

          users.groups.klipper = {
            gid = 321;
          };

          services.klipper = {
            enable = true;

            user = "klipper";
            group = "klipper";

            package = pkgs.klipper;

            settings =
              let
                indentGcode = with lib; gcode:
                  "\n" + (concatMapStringsSep "\n" (x: "    " + x) (splitString "\n" gcode));
              in {
                stepper_x =
                  { step_pin = "P2.2";
                    dir_pin = "!P2.6";
                    enable_pin = "!P2.1";
                    rotation_distance = "40";
                    microsteps = "16";
                    endstop_pin = "P1.29";  # P1.28 for X-max
                    position_endstop = "0";
                    position_max = "235";
                    homing_speed = "50";
                  };

                stepper_y =
                  { step_pin = "P0.19";
                    dir_pin = "!P0.20";
                    enable_pin = "!P2.8";
                    rotation_distance = "40";
                    microsteps = "16";
                    endstop_pin = "P1.27";  # P1.26 for Y-max
                    position_endstop = "0";
                    position_max = "235";
                    homing_speed = "50";
                  };

                stepper_z =
                  {
                    step_pin = "P0.22";
                    dir_pin = "P2.11";
                    enable_pin = "!P0.21";
                    rotation_distance = "8";
                    microsteps = "16";
                    endstop_pin = "P1.25";  # P1.24 for Z-max"
                    position_min = "-4.5";
                    position_endstop = "1.275";
                    position_max = "250";
                  };

                extruder =
                  { step_pin = "P2.13";
                    dir_pin = "!P0.11";
                    enable_pin = "!P2.12";
                    rotation_distance = "23.291";
                    gear_ratio = "3:1";
                    microsteps = "16";
                    nozzle_diameter = "0.400";
                    filament_diameter = "1.750";
                    heater_pin = "P2.7";
                    sensor_type = "PT1000";
                    sensor_pin = "P0.24";
                    control = "pid"; 
                    pid_Kp = "22.2";
                    pid_Ki = "1.08";
                    pid_Kd = "114";
                    min_temp = "0";
                    max_temp = "260";
                    pressure_advance = "0.92";
                  };

                bed_screws =
                  { screw1 = "30,35";
                    screw2 = "200,35";
                    screw3 = "200,205";
                    screw4 = "30,205";
                  };

                "heater_fan my_nozzle_fan" =
                  { pin = "P2.4";
                    heater = "extruder";
                    heater_temp = "50.0";
                    fan_speed = "1.0";
                  };

                heater_bed =
                  { heater_pin = "P2.5";
                    sensor_type = "ATC Semitec 104GT-2";
                    sensor_pin = "P0.23";
                    control = "watermark";
                    min_temp = "0";
                    max_temp = "80";
                  };

                fan =
                  { pin = "P2.3";
                  };

                mcu =
                  { serial = "/dev/serial/by-id/usb-Klipper_lpc1768_13E0FF0C469027AEBAA84A52871E00F5-if00 ";
                  };

                printer =
                  { kinematics = "cartesian";
                    max_velocity = "200";
                    max_accel = "2000";
                    max_z_velocity = "25";
                    max_z_accel = "100";
                  };

                virtual_sdcard =
                  { path = "/var/lib/klipper/sdcard";
                  };

                ### Mainsail
                pause_resume = {};
                display_status = {};

                "tmc2208 stepper_x" =
                  { uart_pin = "P1.17";
                    run_current = "0.475";
                    hold_current = "0.275";
                    stealthchop_threshold = "250";
                  };

                "tmc2208 stepper_y" =
                  { uart_pin = "P1.15";
                    run_current = "0.475";
                    hold_current = "0.275";
                    stealthchop_threshold = "250";
                  };

                "tmc2208 stepper_z" =
                  { uart_pin = "P1.10";
                    run_current = "0.475";
                    hold_current = "0.275";
                    stealthchop_threshold = "30";
                  };

                "tmc2208 extruder" =
                  { uart_pin = "P1.8";
                    run_current = "0.560";
                    hold_current = "0.360";
                    stealthchop_threshold = "5";
                  };

                board_pins =
                  { aliases = indentGcode
                    ''
                      # EXP1 header
                      EXP1_1=P1.30, EXP1_3=P1.18, EXP1_5=P1.20, EXP1_7=P1.22, EXP1_9=<GND>,
                      EXP1_2=P0.28, EXP1_4=P1.19, EXP1_6=P1.21, EXP1_8=P1.23, EXP1_10=<5V>,
                      # EXP2 header
                      EXP2_1=P0.17, EXP2_3=P3.26, EXP2_5=P3.25, EXP2_7=P1.31, EXP2_9=<GND>,
                      EXP2_2=P0.15, EXP2_4=P0.16, EXP2_6=P0.18, EXP2_8=<RST>, EXP2_10=<NC>
                      # Pins EXP2_1, EXP2_6, EXP2_2 are also MISO, MOSI, SCK of bus "ssp0"
                    '';
                  };

                display =
                  { lcd_type = "st7920";
                    cs_pin = "EXP1_7";
                    sclk_pin = "EXP1_6";
                    sid_pin = "EXP1_8";
                    encoder_pins = "^EXP1_5, ^EXP1_3";
                    click_pin = "^!EXP1_2";
                  };

                # "endstop_phase stepper_z" =
                #   { endstop_phase = "29";
                #   };

                # "endstop_phase stepper_y" =
                #   { endstop_phase = "57";
                #   };

                # "endstop_phase stepper_x" =
                #   { endstop_phase = "3";
                #   };

                "gcode_macro M600" =
                  { gcode = indentGcode
                      ''
                        {% set x = params.X|default(50)|float %}
                        {% set y = params.Y|default(0)|float %}
                        {% set z = params.Z|default(10)|float %}
                        SAVE_GCODE_STATE NAME=M600_state
                        PAUSE
                        G91
                        G1 E-.8 F2700
                        G1 Z{z}
                        G90
                        G1 X{x} Y{y} F3000
                        G91
                        G1 E-50  F1000
                        G1 X0.1  F3000
                        G1 E-50  F1000
                        G1 X-0.1 F3000
                        G1 E-50  F1000
                        G1 X0.1  F3000
                        G1 E-50  F1000
                        G1 X-0.1 F3000
                        G1 E-50  F1000
                        G1 X0.1  F3000
                        G1 E-50  F1000
                        G1 X-0.1 F3000
                        RESTORE_GCODE_STATE NAME=M600_state
                      '';
                  };

                "gcode_macro CANCEL_PRINT" = {
                  rename_existing = "BASE_CANCEL_PRINT";
                  gcode = indentGcode
                    ''
                      TURN_OFF_HEATERS
                      CLEAR_PAUSE
                      SDCARD_RESET_FILE
                      BASE_CANCEL_PRINT
                    '';
                };


                "gcode_macro PARK_WAIT" =
                  { gcode = indentGcode
                      ''
                        {% set x = params.X|default(0)|float %}
                        {% set y = params.Y|default(230)|float %}
                        {% set z = params.Z|default(10)|float %}
                        {% set e = params.Z|default(20)|float %}
                        {% set millis = params.MILLIS|default(5)|float %}

                        SAVE_GCODE_STATE NAME=PAUSE_state
                        G91
                        G1 E-{e} F2100
                        G1 Z{z}
                        G90
                        G1 X{x} Y{y} F6000

                        G4 P{millis}

                        G91
                        G1 E{e} F2100
                        G90
                        RESTORE_GCODE_STATE NAME=PAUSE_state MOVE=1
                      '';
                  };

                "gcode_macro PAUSE" =
                  { rename_existing = "BASE_PAUSE";
                    gcode = indentGcode
                      ''
                        {% set x = params.X|default(0)|float %}
                        {% set y = params.Y|default(230)|float %}
                        {% set z = params.Z|default(10)|float %}
                        {% set e = params.E|default(20)|float %}

                        SAVE_GCODE_STATE NAME=PAUSE_state
                        BASE_PAUSE
                        G91
                        G1 E-{e} F2100
                        G1 Z{z}
                        G90
                        G1 X{x} Y{y} F6000
                      '';
                  };

                "gcode_macro RESUME" =
                  { rename_existing = "BASE_RESUME";
                    gcode = indentGcode
                      ''
                        {% set e = params.Z|default(20)|float %}

                        G91
                        G1 E{e} F2100
                        G90
                        RESTORE_GCODE_STATE NAME=PAUSE_state MOVE=1
                        BASE_RESUME
                      '';
                  };

                "gcode_macro PRIME_LINE" =
                  { gcode = indentGcode
                    ''
                      G92 E0 # Reset Extruder
                      G1 Z2.0 F3000 # Move Z Axis up little to prevent scratching of Heat Bed
                      G1 X0.1 Y20 Z0.3 F5000.0 # Move to start position
                      G1 X0.1 Y200.0 Z0.3 F1500.0 E15 # Draw the first line
                      G1 X0.4 Y200.0 Z0.3 F5000.0 # Move to side a little
                      G1 X0.4 Y20 Z0.3 F1500.0 E30 # Draw the second line
                      G92 E0 # Reset Extruder
                      G1 Z2.0 F3000 # Move Z Axis up little to prevent scratching of Heat Bed
                      G1 X5 Y20 Z0.3 F5000.0 # Move over to prevent blob squish
                    '';
                  };

                "gcode_macro START_PRINT" =
                  { gcode = indentGcode
                    ''
                      {% set z = params.Z|default(0)|float %}

                      # Use absolute coordinates
                      G90
                      # Reset the G-Code Z offset (adjust Z offset if needed)
                      SET_GCODE_OFFSET Z={z}
                      # Home the printer
                      G28
                      # Prime line
                      G0 Z0
                      PRIME_LINE
                    '';
                  };

                "gcode_macro END_PRINT" =
                  { gcode = indentGcode
                    ''
                      G91 # Relative positioning
                      G1 E-2 F2700 # Retract a bit
                      G1 E-2 Z0.2 F2400 # Retract and raise Z
                      G1 X5 Y5 F3000 # Wipe out
                      G1 Z10 #Raise Z more
                      G90 # Absolute positionning

                      G1 X0 Y200 # Present print
                      M106 S0 # Turn-off fan
                      M104 S0 # Turn-off hotend
                      M140 S0 # Turn-off bed

                      M84 X Y E # Disable all steppers but Z
                    '';
                  };
              };
          };
        }
    )
    ({ ... }:
      {
        networking = {
          hostName = "blowhole";
          useDHCP = false;
          interfaces.eno1.useDHCP = true;

          firewall = {
           enable = true;

           allowedTCPPorts =
             [ 80
               ## Nomad
               4646 4647 4648
               ## Consul
               8600 # DNS
               8500 # HTTP
               8502 # gRPC
               8300 # server
               8301 # LAN serf
               8302 # WAN serf
               ## Vault
               8200
               ## NFS
               111  2049 4000 4001 4002 20048
             ];
           allowedTCPPortRanges =
             [ { from = 21000;
                 to = 21999;
               }
             ];
           allowedUDPPorts =
             [ ## Consul
               8600 # DNS
               8301 # LAN serf
               8302 # WAN serf
               ## NFS
               111  2049 4000 4001 4002 20048
             ];
           allowedUDPPortRanges =
             [ { from = 21000;
                 to = 21999;
               }
             ];
          };
          hostId = "2cb135ac";
        };

        time.timeZone = "Europe/Bratislava";
        system.stateVersion = "21.05";

        security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];
      })
  ];
}
