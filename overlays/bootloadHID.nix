# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "bootloadHID";
  overlay = {bootloadHID}: final: prev: {
    bootloadHID = prev.stdenv.mkDerivation {
      pname = "bootloadHID";
      version = "2012-12-08";

      src = bootloadHID;

      buildInputs = with prev; [
        libusb-compat-0_1
      ];

      sourceRoot = "source/commandline";
      installPhase = ''
            install -d $out/bin
            install bootloadHID $out/bin
          '';
    };
  };
}
