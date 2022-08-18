# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "webcord";
  overlay = {webcord-flake}:
    final:
    prev:
    {
      webcord = webcord-flake.packages.${prev.stdenv.system}.default;
    };
}
