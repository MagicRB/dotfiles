# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{lib, ...}:
with lib; {
  options = {
    passwordHashes = mkOption {
      description = ''
        Password hashes.
      '';
      type = with types; attrsOf (oneOf [str (attrsOf str)]);
      default = {};
    };
  };
}
