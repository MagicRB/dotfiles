# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "mainsail";
  overlay = {}: final: prev: {
    mainsail = final.callPackage ./mainsail.nix {};
  };
}
