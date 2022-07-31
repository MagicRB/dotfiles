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
  cfg = config.mounts;
in {
  options.mounts = mkOption {
    type = with types; attrsOf (attrsOf unspecified);
    description = ''
      Mounts
    '';
  };
}
