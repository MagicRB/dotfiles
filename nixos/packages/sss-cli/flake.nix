{
  inputs = {
    sss-cli = {
      flake = false;
      url = "github:dsprenkels/sss-cli";
    };
  };

  outputs = { self, nixpkgs, sss-cli, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {
      overlay = system: final: prev:
        let
          pkgs = import nixpkgs { inherit system; };
        in
          with final; {
            sss-cli = pkgs.rustPlatform.buildRustPackage {
              pname = "sss-cli";

              version = "0.1.0";
              cargoSha256 = "sha256-ekoeNLDkTtVcBHKcqndEIOtz5jEAXu8cYVbdar5X288=";
              src = sss-cli;
            };
          };
      
      defaultPackage = forAllSystems (system: (import nixpkgs {
        inherit system;
        overlays = [ (self.overlay system) ];
      }).sss-cli);
  };
}
