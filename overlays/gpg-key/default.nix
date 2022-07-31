# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "gpg-key-rb";
  overlays = ["nixng"];
  overlay = {}: final: prev: {
    gpg-key-rb = final.writeSubstitutedShellScriptBin {
      name = "gpg-key";
      file = ./gpg-key;
      substitutes = with prev; {
        inherit cryptsetup busybox findutils gnupg sudo;
      };
    };
  };
}
