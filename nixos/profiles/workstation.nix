# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  lib,
  roots,
  inputs,
  ...
}:
with lib; {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    (roots.nixos + "/modules")
  ];

  magic_rb = {
    flakes.enable = true;
    sshdEmacs.enable = true;
    pulseaudio.enable = true;

    serokell = true;

    xserver = {
      enable = true;
      xmonad = true;
      qwertyNeo2 = true;
      mimickInTty = true;
    };
  };

  home-manager.users."main" = {...}: {
    imports = [
      (roots.home-manager + "/modules")
    ];

    magic_rb = {
      programs = {
        alacritty.enable = true;
        bash = {
          enable = true;
          enableDirenv = true;
        };
        ssh.enable = true;
        emacs = {
          enable = true;
          enableMu4e = true;
        };
        xmonad.enable = true;
        gpg.enable = true;
        multimc.enable = true;
      };

      packageCollections = {
        "3dPrinting".enable = true;
        cmdline.enable = true;
        graphical.enable = true;
        webdev.enable = true;
      };
    };
  };

  nixpkgs.config.allowUnfree = true;
  services.sshd.enable = true;

  # Virtualisation
  virtualisation.docker.enable = true;
  virtualisation.containerd.enable = true;

  ## Fuck podman, 2021-08-31, `podman info` fails with a stack trace
  ## and all containers tested fail with `Operation not supported`...
  # virtualisation.podman = {
  #   enable = true;
  #   dockerCompat = true;
  # };

  # System emulation
  boot.binfmt.emulatedSystems = [
    "aarch64-linux"
    "riscv64-linux"
  ];

  time.timeZone = "Europe/Bratislava";
  security.pki.certificates =
    singleton (builtins.readFile (roots.flake + "/redalder.org.crt"));
}
