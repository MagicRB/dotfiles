sss-cli:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    sss-cli = prev.rustPlatform.buildRustPackage {
      pname = "sss-cli";

      version = "0.1.0";
      cargoSha256 = "sha256-ekoeNLDkTtVcBHKcqndEIOtz5jEAXu8cYVbdar5X288=";
      src = sss-cli;
    };
  };
}
