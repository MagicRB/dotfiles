# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  lib,
  roots,
  ...
}:
with lib; {
  imports = [
    (roots.nixos + "/profiles/vps.nix")
  ];

  magic_rb.grub = {
    enable = true;
    efi.enable = true;
    devices = ["nodev"];
  };

  networking.interfaces.ens3.useDHCP = true;

  time.timeZone = "Europe/Bratislava";
  security.pki.certificates =
    singleton (builtins.readFile (roots.flake + "/redalder.org.crt"));
}
