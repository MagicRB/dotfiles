# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{lib, ...}:
with lib; {
  options = {
    network.ips = mkOption {
      description = ''
        Host IPs.
      '';
      type = with types; attrsOf (oneOf [str (attrsOf str)]);
      default = {};
    };

    network.networks = mkOption {
      description = ''
        Network IPs.
      '';
      type = with types; attrsOf (oneOf [str (attrsOf str)]);
      default = {};
    };
  };
}
