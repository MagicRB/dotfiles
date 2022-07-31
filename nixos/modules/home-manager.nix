# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  roots,
  inputs,
  lib,
  options,
  ...
}:
with lib; {
  config =
    optionalAttrs (options ? "home-manager")
    {
      home-manager.useGlobalPkgs = true;
      home-manager.extraSpecialArgs = {inherit roots inputs;};
    };
}
