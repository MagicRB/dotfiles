# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "zfs-relmount";
  overlay = {}: final: prev: {
    zfs-relmount =
      prev.writeShellScriptBin "zfs-relmount"
      (builtins.readFile ./zfs-relmount.sh);
  };
}
