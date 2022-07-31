{qmk}: {
  avr ? true,
  arm ? true,
  teensy ? true,
  pkgsCross,
  mkShell,
  lib,
  poetry2nix,
  stdenv,
  fetchurl,
  fetchFromGitHub,
  clang-tools,
  dfu-programmer,
  dfu-util,
  diffutils,
  git,
  which,
  avrBinutils' ? pkgsCross.avr.buildPackages.binutils,
  avrGcc' ? pkgsCross.avr.buildPackages.gcc8,
  avrlibc' ? pkgsCross.avr.libcCross,
  avrdude,
  gcc-arm-embedded,
  teensy-loader-cli,
  libusb-compat-0_1,
}: let
  avr_incflags = [
    "-isystem ${avrlibc'}/avr/include"
    "-B${avrlibc'}/avr/lib/avr5"
    "-L${avrlibc'}/avr/lib/avr5"
    "-B${avrlibc'}/avr/lib/avr35"
    "-L${avrlibc'}/avr/lib/avr35"
    "-B${avrlibc'}/avr/lib/avr51"
    "-L${avrlibc'}/avr/lib/avr51"
  ];

  pythonEnv = poetry2nix.mkPoetryEnv {
    projectDir = "${qmk}/util/nix";
    overrides = poetry2nix.overrides.withDefaults (self: super: {
      qmk = super.qmk.overridePythonAttrs (old: {
        # Allow QMK CLI to run "bin/qmk" as a subprocess (the wrapper changes
        # $PATH and breaks these invocations).
        dontWrapPythonPrograms = true;
      });
    });
  };
in rec {
  bootloadhid = stdenv.mkDerivation rec {
    pname = "bootloadhid";
    version = "2012-12-08";

    src = fetchurl {
      url = "https://www.obdev.at/downloads/vusb/bootloadHID.2012-12-08.tar.gz";
      sha256 = "sha256-FU5+OGKaOi7sLfZm7foe4vLppXAY8X2fD48GTMINh1Q=";
    };

    sourceRoot = "bootloadHID.${version}/commandline";

    nativeBuildInputs = [libusb-compat-0_1.dev];

    installPhase = ''
      mkdir -p $out/bin
      cp bootloadHID $out/bin
    '';
  };

  shell = mkShell {
    name = "qmk-devshell";

    buildInputs =
      [clang-tools dfu-programmer dfu-util diffutils git pythonEnv bootloadhid]
      ++ lib.optional avr [
        avrBinutils'
        avrGcc'
        avrlibc'
        avrdude
      ]
      ++ lib.optional arm [gcc-arm-embedded]
      ++ lib.optional teensy [teensy-loader-cli];

    AVR_CFLAGS = lib.optional avr avr_incflags;
    AVR_ASFLAGS = lib.optional avr avr_incflags;
    shellHook = ''
      # Prevent the avr-gcc wrapper from picking up host GCC flags
      # like -iframework, which is problematic on Darwin
      unset NIX_CFLAGS_COMPILE_FOR_TARGET
    '';
  };

  qmk-firmware = stdenv.mkDerivation {
    name = "qmk-firmware";

    src = fetchFromGitHub {
      owner = "qmk";
      repo = "qmk_firmware";
      fetchSubmodules = true;
      leaveDotGit = true;
      sha256 = "sha256-bd+NOzwqQCDBw2FYYtGWpmja0tFhsfpdDFyZXlSM/U4=";
      rev = "0.16.5";
    };

    configurePhase = ''
      mkdir -p keyboards/mt/split75/keymaps/custom
      cp ${./keymap.c} keyboards/mt/split75/keymaps/custom/keymap.c
      cp ${./rules.mk} keyboards/mt/split75/rules.mk
      cp ${./config.h} keyboards/mt/split75/keymaps/custom/config.h
    '';

    buildPhase = ''
      make mt/split75:custom
    '';

    nativeBuildInputs =
      [clang-tools dfu-programmer dfu-util diffutils git pythonEnv bootloadhid which]
      ++ lib.optional avr [
        avrBinutils'
        avrGcc'
        avrlibc'
        avrdude
      ]
      ++ lib.optional arm [gcc-arm-embedded]
      ++ lib.optional teensy [teensy-loader-cli];
    AVR_CFLAGS = lib.optional avr avr_incflags;
    AVR_ASFLAGS = lib.optional avr avr_incflags;
    shellHook = ''
      # Prevent the avr-gcc wrapper from picking up host GCC flags
      # like -iframework, which is problematic on Darwin
      unset NIX_CFLAGS_COMPILE_FOR_TARGET
    '';

    installPhase = ''
      cp wheatfield_split75_custom.hex $out
    '';
  };
}
