self: super:
let
  version = "0.0.13";
in {
  discord = (super.discord.overrideAttrs ( old: {
    inherit version;
    src = super.fetchurl {
      url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "0d5z6cbj9dg3hjw84pyg75f8dwdvi2mqxb9ic8dfqzk064ssiv7y";
    };
  }));
}
