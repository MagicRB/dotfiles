# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  fetchurl,
  lib,
  unzip,
}:
with lib;
  fetchurl {
    url = "https://github.com/mainsail-crew/mainsail/releases/download/v2.2.1/mainsail.zip";
    sha256 = "sha256-GpL1vEJZGNaP88zltiuOv0TE/vuc59FVk8HPcaA69ho=";
    downloadToTemp = true;
    recursiveHash = true;
    name = "mainsail";
    postFetch = ''
      mkdir -p $out
      ${unzip}/bin/unzip -d $out $downloadedFile
    '';
  }
