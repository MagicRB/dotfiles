# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{...}: {
  imports = [
    ./systems.nix
    ./overlays.nix
    ./devShells.nix
    # Doesn't build
    # ./qmk
  ];
}
