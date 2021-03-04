{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs, sss-cli, rlib, ... }@inputs:
    {
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
      
      defaultPackage = rlib.forAllSystems (system: (import nixpkgs {
        inherit system;
        overlays = [ (self.overlay system) ];
      }).sss-cli);
  };
}
