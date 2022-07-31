# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{roots, ...}: {
  imports = [
    "${roots.flake}/nixos/secret-lib"
    "${roots.nixos}/hardware/default.nix"
    ./template-files.nix
    ./efi-grub.nix
    ./erase-my-darlings.nix
    ./main.nix
    ./networking.nix
    ./nix-flakes.nix
    ./optimisation.nix
    ./pulseaudio.nix
    ./vault-agent.nix
    ./vps-remote-access.nix
    ./sshd-emacs.nix
    ./xserver
    ./home-manager.nix
    ./serokell.nix
    ./gaming.nix
    ./hashicorp.nix
  ];
}
