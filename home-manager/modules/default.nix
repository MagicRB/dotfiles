# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{roots, ...}: {
  imports = [
    ./alacritty
    ./bash
    ./ssh
    ./emacs
    ./xmonad
    ./3d-printing.nix
    ./cmdline-utils.nix
    ./gpg.nix
    ./graphical-programs.nix
    ./multimc.nix
    ./optimisation.nix
    ./webdev.nix
    ./wine.nix
    (roots.flake + "/nixos/secret-lib")
  ];
}
