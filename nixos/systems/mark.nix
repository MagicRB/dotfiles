# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  system = "x86_64-linux";
  name = "mark";
  module = {
    pkgs,
    config,
    roots,
    ...
  }: {
    imports = [
      (roots.nixos + "/modules")
    ];

    magic_rb = {
      hardware.mark = true;
    };
    time.timeZone = "Europe/Bratislava";
    system.stateVersion = "20.09";

    environment.systemPackages = with pkgs; [
      gnupg
      pinentry
      openssl
      paperkey
      monkeysphere
      ssss
    ];
  };
}
