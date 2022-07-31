# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "screenshot";
  overlays = ["nixng"];
  overlay = {}: final: prev: {
    magic_rb =
      prev.magic_rb
      or {}
      // {
        screenshot = final.writeSubstitutedShellScriptBin {
          name = "screenshot";
          file = ./screenshot;
          substitutes = with prev; {
            inherit busybox scrot xclip;
          };
        };
      };
  };
}
