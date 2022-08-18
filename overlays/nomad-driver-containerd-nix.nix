# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "nomad-driver-containerd-nix";
  overlay = {nomad-driver-containerd-nix}:
    final:
    prev:
    {
      nomad-driver-containerd-nix =
        (nomad-driver-containerd-nix.overlay final prev).nomad-driver-containerd-nix.overrideAttrs (old:
          { vendorSha256 = "sha256-xLQZzs5WzdWUndKhc4hkVqijewfYY9CipAPCgi39a7M="; }
        );
    };
}
