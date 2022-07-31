# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  name = "nomad-driver-containerd-nix";
  overlay = {nomad-driver-containerd-nix}: nomad-driver-containerd-nix.overlay;
}
