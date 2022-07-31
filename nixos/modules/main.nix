# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  config,
  lib,
  secret,
  ...
}:
with lib; {
  users = {
    mutableUsers = false;

    users.main = {
      isNormalUser = true;
      home = "/home/main";
      hashedPassword = secret.passwordHashes.main.generic;
      description = "main";

      uid = 1000;

      extraGroups = ["wheel" "audio"];

      openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFVkFvalffJ/SMjJGG3WPiqCqFygnWzhGUaeALBIoCsJ (none)"];
    };

    groups.main = {
      gid = 1000;
    };
  };
}
