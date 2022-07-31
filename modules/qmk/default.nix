# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{inputs, ...}: {
  perSystem =
    {lib, pkgs, ...}:
    let
      avrlibc = pkgs.pkgsCross.avr.libcCross;
      avrBinutils = pkgs.pkgsCross.avr.buildPackages.binutils;
      avrGcc = pkgs.pkgsCross.avr.buildPackages.gcc8;

      bootloadHID =
        (((import ../../overlays/bootloadHID.nix).overlay
          { inherit (inputs) bootloadHID; }) null pkgs).bootloadHID;

      avr_incflags = [
        "-isystem ${avrlibc}/avr/include"
        "-B${avrlibc}/avr/lib/avr5"
        "-L${avrlibc}/avr/lib/avr5"
        "-B${avrlibc}/avr/lib/avr35"
        "-L${avrlibc}/avr/lib/avr35"
        "-B${avrlibc}/avr/lib/avr51"
        "-L${avrlibc}/avr/lib/avr51"
      ];

      pythonEnv = with pkgs; poetry2nix.mkPoetryEnv {
        projectDir = "${inputs.qmk}/util/nix";
        overrides = poetry2nix.overrides.withDefaults (self: super: {
          qmk = super.qmk.overridePythonAttrs (old: {
            # Allow QMK CLI to run "bin/qmk" as a subprocess (the wrapper changes
            # $PATH and breaks these invocations).
            dontWrapPythonPrograms = true;
          });
        });
      };
    in
      {
        devShells.qmk = pkgs.mkShell {
          name = "qmk-devshell";

          buildInputs = with pkgs;
            [clang-tools dfu-programmer dfu-util diffutils git pythonEnv bootloadHID
             qmk
             avrBinutils
             avrGcc
             avrlibc
             avrdude
             gcc-arm-embedded
             teensy-loader-cli];

          AVR_CFLAGS = avr_incflags;
          AVR_ASFLAGS = avr_incflags;
          shellHook = ''
      # Prevent the avr-gcc wrapper from picking up host GCC flags
      # like -iframework, which is problematic on Darwin
      unset NIX_CFLAGS_COMPILE_FOR_TARGET
    '';
        };

        packages.qmk = pkgs.stdenv.mkDerivation {
          name = "qmk-firmware";

          src = inputs.qmk;

    configurePhase = ''
      mkdir -p keyboards/mt/split75/keymaps/custom
      cp ${./keymap.c} keyboards/mt/split75/keymaps/custom/keymap.c
      cp ${./rules.mk} keyboards/mt/split75/rules.mk
      cp ${./config.h} keyboards/mt/split75/keymaps/custom/config.h
    '';

    buildPhase = ''
      qmk setup
      make mt/split75:default

    '';

          nativeBuildInputs = with pkgs;
            [clang-tools dfu-programmer dfu-util diffutils git pythonEnv bootloadHID which
             qmk
              avrBinutils
              avrGcc
              avrlibc
              avrdude
              gcc-arm-embedded
              teensy-loader-cli];
          AVR_CFLAGS = avr_incflags;
          AVR_ASFLAGS = avr_incflags;
          shellHook = ''
      # Prevent the avr-gcc wrapper from picking up host GCC flags
      # like -iframework, which is problematic on Darwin
      unset NIX_CFLAGS_COMPILE_FOR_TARGET
    '';

          installPhase = ''
      cp wheatfield_split75_custom.hex $out
    '';
        };
      };
}
