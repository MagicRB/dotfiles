{lib, pkgs, secret, ...}:
with lib; let
in {
  users.users.klipper = {
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

    settings = let
      indentGcode = with lib;
        gcode:
          "\n" + (concatMapStringsSep "\n" (x: "    " + x) (splitString "\n" gcode));
    in {
      stepper_x = {
        step_pin = "P2.2";
        dir_pin = "!P2.6";
        enable_pin = "!P2.1";
        rotation_distance = "40";
        microsteps = "16";
        endstop_pin = "P1.29"; # P1.28 for X-max
        position_endstop = "0";
        position_max = "235";
        homing_speed = "50";
      };

      stepper_y = {
        step_pin = "P0.19";
        dir_pin = "!P0.20";
        enable_pin = "!P2.8";
        rotation_distance = "40";
        microsteps = "16";
        endstop_pin = "P1.27"; # P1.26 for Y-max
        position_endstop = "0";
        position_max = "235";
        homing_speed = "50";
      };

      stepper_z = {
        step_pin = "P0.22";
        dir_pin = "P2.11";
        enable_pin = "!P0.21";
        rotation_distance = "8";
        microsteps = "16";
        endstop_pin = "P1.25"; # P1.24 for Z-max"
        position_min = "-4.5";
        position_endstop = "1.290";
        position_max = "250";
      };

      extruder = {
        step_pin = "P2.13";
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

      bed_screws = {
        screw1 = "30,35";
        screw2 = "200,35";
        screw3 = "200,205";
        screw4 = "30,205";
      };

      "heater_fan my_nozzle_fan" = {
        pin = "P2.4";
        heater = "extruder";
        heater_temp = "50.0";
        fan_speed = "1.0";
      };

      heater_bed = {
        heater_pin = "P2.5";
        sensor_type = "ATC Semitec 104GT-2";
        sensor_pin = "P0.23";
        control = "watermark";
        min_temp = "0";
        max_temp = "80";
      };

      fan = {
        pin = "P2.3";
      };

      mcu = {
        serial = "/dev/serial/by-id/usb-Klipper_lpc1768_13E0FF0C469027AEBAA84A52871E00F5-if00 ";
      };

      printer = {
        kinematics = "cartesian";
        max_velocity = "200";
        max_accel = "2000";
        max_z_velocity = "25";
        max_z_accel = "100";
      };

      virtual_sdcard = {
        path = "/var/lib/klipper/sdcard";
      };

      ### Mainsail
      pause_resume = {};
      display_status = {};
      endstop_phase = {};

      "tmc2208 stepper_x" = {
        uart_pin = "P1.17";
        run_current = "0.475";
        hold_current = "0.275";
        stealthchop_threshold = "250";
      };

      "tmc2208 stepper_y" = {
        uart_pin = "P1.15";
        run_current = "0.475";
        hold_current = "0.275";
        stealthchop_threshold = "250";
      };

      "tmc2208 stepper_z" = {
        uart_pin = "P1.10";
        run_current = "0.475";
        hold_current = "0.275";
        stealthchop_threshold = "30";
      };

      "tmc2208 extruder" = {
        uart_pin = "P1.8";
        run_current = "0.560";
        hold_current = "0.360";
        stealthchop_threshold = "5";
      };

      board_pins = {
        aliases =
          indentGcode
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

      display = {
        lcd_type = "st7920";
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

      "gcode_macro M600" = {
        gcode =
          indentGcode
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
        gcode =
          indentGcode
          ''
            TURN_OFF_HEATERS
            CLEAR_PAUSE
            SDCARD_RESET_FILE
            BASE_CANCEL_PRINT
          '';
      };

      "gcode_macro PARK_WAIT" = {
        gcode =
          indentGcode
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

      "gcode_macro PAUSE" = {
        rename_existing = "BASE_PAUSE";
        gcode =
          indentGcode
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

      "gcode_macro RESUME" = {
        rename_existing = "BASE_RESUME";
        gcode =
          indentGcode
          ''
            {% set e = params.Z|default(20)|float %}

            G91
            G1 E{e} F2100
            G90
            RESTORE_GCODE_STATE NAME=PAUSE_state MOVE=1
            BASE_RESUME
          '';
      };

      "gcode_macro PRIME_LINE" = {
        gcode =
          indentGcode
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

      "gcode_macro START_PRINT" = {
        gcode =
          indentGcode
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

      "gcode_macro END_PRINT" = {
        gcode =
          indentGcode
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

  services.moonraker = {
    enable = true;

    settings = {
      authorization = {
        trusted_clients = with secret.network.ips; [
          "127.0.0.1"
          heater
          edge.vpn
          omen.vpn
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
      root = pkgs.mainsail;

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
}
