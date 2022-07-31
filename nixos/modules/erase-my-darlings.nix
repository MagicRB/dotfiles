# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.magic_rb.erase-my-darlings;
in
  # Send love to https://grahamc.com/blog/erase-your-darlings
  {
    options.magic_rb.erase-my-darlings = {
      zfs = {
        enable = mkEnableOption "Erase the root filesystem using ZFS snapshots on boot.";

        snapshot = mkOption {
          type = types.str;
          description = "Which snapshot to rollback to, also specifies which dataset to rollback.";
        };
      };

      btrfs = {
        enable = mkEnableOption "Erase the root filesystem using BTRFS snapshots on boot.";

        snapshot = mkOption {
          type = types.str;
          description = "Which snapshot to rollback to.";
        };

        target = mkOption {
          type = types.str;
          description = "Which subvolume to rollback.";
        };

        disk = mkOption {
          type = types.str;
          description = "Which disk contains these subvolumes.";
        };
      };
    };

    config =
      mkIf (cfg.zfs.enable || cfg.btrfs.enable)
      {
        boot.initrd.postDeviceCommands =
          mkAfter
          ((optionalString cfg.zfs.enable ''
              zfs rollback -r ${cfg.zfs.snapshot}
            '')
            + (optionalString cfg.btrfs.enable ''
              waitDevice "${cfg.btrfs.disk}"

              mkdir -p /mnt

              mount -o subvol=/ ${cfg.btrfs.disk} /mnt

              btrfs subvolume list -o /mnt/${cfg.btrfs.target} |
              cut -f9 -d' ' |
              while read subvolume; do
                echo "deleting /$subvolume subvolume..."
                btrfs subvolume delete "/mnt/$subvolume"
              done &&
              echo "deleting ${cfg.btrfs.target} subvolume..." &&
              btrfs subvolume delete /mnt/${cfg.btrfs.target}

              echo "restoring ${cfg.btrfs.snapshot} to ${cfg.btrfs.target}..."
              btrfs subvolume snapshot /mnt/${cfg.btrfs.snapshot} /mnt/${cfg.btrfs.target}

              umount /mnt
            ''));

        assertions = [
          {
            assertion =
              (cfg.zfs.enable == true && cfg.btrfs.enable == false)
              || (cfg.zfs.enable == false && cfg.btrfs.enable == true)
              || (cfg.zfs.enable == false && cfg.btrfs.enable == false);
            message = "Erase my darlings can be enable either with zfs, btrfs or none, not both.";
          }
        ];
      };
  }
