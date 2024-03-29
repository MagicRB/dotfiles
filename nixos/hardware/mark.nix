# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.magic_rb.hardware.mark;
in {
  options.magic_rb.hardware.mark = mkEnableOption "Enable mark.";

  config = mkIf cfg {
    boot = {
      loader.grub = {
        enable = true;
        version = 2;
        device = "/dev/disk/by-id/ata-ST3160812AS_9LS6TXFN";
        efiSupport = false;
        enableCryptodisk = true;
      };

      initrd = {
        luks.devices = {
          root = {
            device = "/dev/disk/by-uuid/bfbebbc9-b361-4172-b29e-34bacb7a1d4c";
            preLVM = true;
          };

          boot = {
            device = "/dev/disk/by-uuid/ec48314c-8cc9-433f-a711-873418cb893c";
            preLVM = true;
          };
        };

        availableKernelModules = [
          "xhci_pci"
          "ahci"
          "usbhid"
          "aesni_intel"
          "cryptd"
        ];
      };
    };

    fileSystems = {
      "/" = {
        device = "/dev/mapper/root";
        fsType = "ext4";
      };

      "/boot" = {
        device = "/dev/mapper/boot";
        fsType = "ext4";
      };
    };
  };
}
