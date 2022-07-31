# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "uboot-clara-hd";
  overlay = {}: final: prev: {
    # magic_rb = prev.magic_rb or {} // {
    #   uboot-clara-hd = with prev;
    #     let crossPkgs =
    #           import prev.src
    #             { crossSystem.config = "arm-linux-gnueabihf";
    #             };
    #     in
    #       stdenv.mkDerivation {
    #         pname = "uboot-clara-hd";
    #         version = "??";
    #         src = fetchFromGitHub
    #           { owner = "akemnade";
    #             repo = "u-boot-fslc";
    #             rev = "2021.04+fslc";
    #             sha256 = "sha256-P4aT6BX875sSIRLrk6BKfgkdBGRD8UbIINRf6PdG4hA";
    #           };
    #         nativeBuildInputs = [ bison flex crossPkgs.stdenv.cc ];
    #         makeFlags = "ARCH=arm CROSS_COMPILE=arm-linux-gnueabihf- mx6sllclarahd_defconfig";
    #       };
    # };
  };
}
