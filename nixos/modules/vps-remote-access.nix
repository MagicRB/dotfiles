# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.magic_rb.vpsRemoteAccess;
in {
  options.magic_rb = {
    vpsRemoteAccess = {
      enable = mkEnableOption "Enable VPS remote access module.";
      trustedWheel = mkEnableOption "Add the wheel group to Nix trusted-users.";
    };
  };

  config =
    mkIf cfg.enable
    {
      nix.settings.trusted-users =
        mkIf cfg.trustedWheel
        ["@wheel"];

      services.openssh = {
        enable = true;
        passwordAuthentication = false;
        permitRootLogin = "no";
      };
    };
}
