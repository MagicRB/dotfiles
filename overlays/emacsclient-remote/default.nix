# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "emacsclient-remote";
  overlay = {}: final: prev:
    with final; {
      magic_rb =
        prev.magic_rb
        or {}
        // {
          emacsclient-remote =
            prev.writeShellScriptBin
            "emacsclient-remote" (builtins.readFile ./emacsclient-remote);
        };
    };
}
