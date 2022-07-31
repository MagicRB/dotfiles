# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{...}: {
  perSystem = {pkgs, ...}: {
    devShells.default = pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
        lefthook
        alejandra
        reuse
        terraform
        vault
        nomad
        consul
        dnsutils
      ];
    };
  };
}
