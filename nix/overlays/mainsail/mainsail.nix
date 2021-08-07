{ fetchurl
, lib
, unzip
}:
with lib;
fetchurl {
  url = "https://github.com/meteyou/mainsail/releases/download/v1.6.0/mainsail.zip";
  sha256 = "sha256-tg3wuJOe5umh6ZiRRdyXABPxhSER/BYvWBd5xuKdjb4=";
  downloadToTemp = true;
  recursiveHash = true;
  name = "mainsail";
  postFetch =
    ''
      mkdir -p $out
      ${unzip}/bin/unzip -d $out $downloadedFile
    '';
}
