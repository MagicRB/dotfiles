# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "fufexan-discord-canary";
  systems = [ "x86_64-linux" ];
  overlay = {
    fufexan-dotfiles,
    nixpkgs,
  } @ inputs: final: prev: {
    fufexan-discord-canary = (import (fufexan-dotfiles + "/pkgs/default.nix") inputs final prev).discord-electron-openasar.override {
      isWayland = false;
      inherit (prev.discord-canary) src pname version;
      binaryName = "DiscordCanary";
    };
  };
}
