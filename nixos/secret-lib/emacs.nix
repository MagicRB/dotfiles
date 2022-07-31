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
  options.emacs = {
    mbsyncrc = mkOption {
      type = types.path;
    };

    mu4eContexts = mkOption {
      type = types.path;
    };
  };
}
