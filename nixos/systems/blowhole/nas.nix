{ pkgs, ... }:
{
  fileSystems."/mnt/cartman" = {
    device = "storfa/ds1/cartman";
    fsType = "zfs";
  };
  systemd.services.mnt-kyle-zfs-relmount = {
    requires = ["mnt-kyle.mount"];
    after = ["mnt-kyle.mount"];

    path = with pkgs; [zfs utillinux];

    serviceConfig = {
      RemainAfterExit = true;
      Type = "oneshot";
      ExecStart = "${pkgs.zfs-relmount}/bin/zfs-relmount mount storfa/ds1/kyle /mnt/kyle";
    };
  };

  fileSystems."/mnt/kyle" = {
    device = "storfa/ds1/kyle";
    fsType = "zfs";
  };

  systemd.services.mnt-cartman-zfs-relmount = {
    requires = ["mnt-cartman.mount"];
    after = ["mnt-cartman.mount"];

    path = with pkgs; [zfs utillinux];

    serviceConfig = {
      RemainAfterExit = true;
      Type = "oneshot";
      ExecStart = "${pkgs.zfs-relmount}/bin/zfs-relmount mount storfa/ds1/cartman /mnt/cartman";
    };
  };

  fileSystems."/mnt/stan" = {
    device = "storfa/ds1/stan";
    fsType = "zfs";
  };

  systemd.services.mnt-stan-zfs-relmount = {
    requires = ["mnt-stan.mount"];
    after = ["mnt-stan.mount"];

    path = with pkgs; [zfs utillinux];

    serviceConfig = {
      RemainAfterExit = true;
      Type = "oneshot";
      ExecStart = "${pkgs.zfs-relmount}/bin/zfs-relmount mount storfa/ds1/stan /mnt/stan";
    };
  };

  # services.samba = {
  #   enable = true;
  #   package = pkgs.sambaFull;
  #   openFirewall = true;

  #   securityType = "user";
  #   extraConfig = ''
  #     workgroup = WORKGROUP
  #     server string = blowhole
  #     netbios name = blowhole
  #     security = user
  #     #use sendfile = yes
  #     #max protocol = smb2
  #     # note: localhost is the ipv6 localhost ::1
  #     hosts allow = 192.168.0. 127.0.0.1 localhost
  #     hosts deny = 0.0.0.0/0
  #     guest account = nobody
  #     map to guest = bad user

  #     # Enable POSIX ACLs
  #     vfs objects = acl_xattr
  #     map acl inherit = yes
  #     store dos attributes = yes
  #     smb2 unix extensions = yes
  #   '';

  #   shares = {
  #     cartman = {
  #       path = "/mnt/cartman";
  #       browseable = "yes";
  #       "read only" = "no";
  #       "guest ok" = "no";
  #       "force user" = "main";
  #       "force group" = "main";
  #       "locking" = "yes";
  #       "valid users" = "main";
  #     };
  #     kyle = {
  #       path = "/mnt/kyle";
  #       browseable = "yes";
  #       "read only" = "no";
  #       "guest ok" = "no";
  #       "force user" = "main";
  #       "force group" = "main";
  #       "locking" = "yes";
  #       "valid users" = "main";
  #     };
  #     stan = {
  #       path = "/mnt/stan";
  #       browseable = "yes";
  #       "read only" = "no";
  #       "guest ok" = "no";
  #       "force user" = "main";
  #       "force group" = "main";
  #       "locking" = "yes";
  #       "valid users" = "main";
  #     };
  #   };
  # };

  fileSystems."/run/restic" = {
    fsType = "tmpfs";
    options = [ "size=64M" ];
  };

  services.restic.backups.cartman = {
    initialize = true;
    timerConfig = {
      OnCalendar = "03:00";
    };

    paths = [ "/run/restic/cartman" ];
    backupPrepareCommand = ''
      snapshot="$(date +restic%+4Y_%U_%u)"
      ${pkgs.zfs-relmount}/bin/zfs-relmount snapshot storfa/ds1/cartman "''${snapshot}"

      mkdir /run/restic/cartman
      ${pkgs.zfs-relmount}/bin/zfs-relmount mount-snapshot storfa/ds1/cartman /run/restic/cartman "''${snapshot}"

      export RESTIC_PROGRESS_FPS=1
    '';
    backupCleanupCommand = ''
      ${pkgs.zfs-relmount}/bin/zfs-relmount umount storfa/ds1/cartman /run/restic/cartman
      rm -r /run/restic/cartman
    '';

    passwordFile = "";
  };

  systemd.timers."restic-backups-cartman" = {
    timerConfig = {
      Persistent = true;
      WakeSystem = true;
    };
  };

  systemd.services."restic-backups-cartman" = {
    path = with pkgs; [
      utillinux
      zfs
    ];
    serviceConfig = {
      Nice = 19;
      IOSchedulingClass = "idle";
      EnvironmentFile = "/var/secrets/restic-b2";
    };
  };
}
