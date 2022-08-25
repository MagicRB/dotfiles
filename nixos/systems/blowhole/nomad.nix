{lib, config, pkgs, secret, ...}:
with lib;
let
in
{
  services.hashicorp.nomad = {
    enable = true;

    extraPackages = with pkgs; [coreutils iproute2 iptables consul glibc config.nix.package git];
    extraSettingsPaths = [
      "/run/secrets/nomad.json"
    ];
    extraPluginPaths = [pkgs.nomad-driver-containerd-nix];
    package = pkgs.nomad_1_3.overrideAttrs (old:
      {
        patches = [
          ../0001-Revert-Change-consul-SI-tokens-to-be-local.patch
        ];
      });

    settings = {
      bind_addr = secret.network.ips.blowhole.ip;
      server = {
        enabled = true;
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
        allow_unauthenticated = true;
        create_from_role = "nomad-cluster";
      };

      consul = {
        address = "${secret.network.ips.blowhole.ip}:8500";
        auto_advertise = true;
        server_auto_join = true;
        client_auto_join = true;
      };

      acl = {
        enabled = true;
      };

      client = {
        cni_path = "${pkgs.cni-plugins}/bin";

        min_dynamic_port = 20000;
        max_dynamic_port = 32000;

        options = {
          "docker.privileged.enabled" = "true";
        };

        host_network."default" = {
          cidr = secret.network.networks.home.mine;
        };

        host_network."mesh" = {
          cidr = secret.network.networks.home.mine;
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

        # host_volume."sonoff" = {
        #   path = "/dev/serial/by-id/usb-ITead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_4c004e9c53c9eb118a9f8b4f1d69213e-if00-port0";
        # };

        enabled = true;
      };

      plugin."docker" = {
        config = {
          allow_caps = [
            "CHOWN"
            "DAC_OVERRIDE"
            "FSETID"
            "FOWNER"
            "MKNOD"
            "NET_RAW"
            "SETGID"
            "SETUID"
            "SETFCAP"
            "SETPCAP"
            "NET_BIND_SERVICE"
            "SYS_CHROOT"
            "KILL"
            "AUDIT_WRITE"
            "SYS_ADMIN"
          ];
          allow_privileged = true;
        };
      };

      plugin."nomad-driver-containerd" = {
        config = {
          enabled = true;
          containerd_runtime = "io.containerd.runc.v2";
          stats_interval = "5s";
        };
      };

      disable_update_check = true;
      data_dir = "/var/lib/nomad";

      datacenter = "homelab-1";
      region = "homelab-1";
    };
  };

  virtualisation.docker.enable = true;
  virtualisation.docker.daemon.settings = {
    dns = [
      "10.64.2.1"
    ];
  };
  virtualisation.containerd.enable = true;
}
