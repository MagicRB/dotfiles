# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  lib,
  roots,
  inputs,
  ...
}:
with lib; {
  imports = [
    (roots.nixos + "/modules")
    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager.users."main" = {config, ...}: {
    imports = [(roots.home-manager + "/modules")];
    magic_rb.programs.bash.enable = true;
  };

  magic_rb = {
    flakes.enable = true;
    sshdEmacs.enable = true;
    vpsRemoteAccess = {
      enable = true;
      trustedWheel = true;
    };
  };

  networking.firewall.enable = true;

  time.timeZone = "Europe/Bratislava";
  security.pki.certificates =
    singleton (builtins.readFile (roots.flake + "/redalder.org.crt"));
}
