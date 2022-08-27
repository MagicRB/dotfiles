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
  cfg = config.emacs;
in {
  options.ical2org = {
    icalUrl = mkOption {
      type = types.str;
    };

    orgPath = mkOption {
      type = types.str;
    };
  };
}
