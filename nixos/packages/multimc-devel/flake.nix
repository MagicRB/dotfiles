{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      {
        overlay = system: final: prev:
          with final;
          let
            # writeShellScriptBin = nixpkgs.writeShellScriptBin;
            pkgs = import nixpkgs { inherit system; };
          in {
            multimc-devel = pkgs.multimc.overrideAttrs (old: rec {
              version = "develop";
              src = fetchFromGitHub {
                owner = "MultiMC";
                repo = "MultiMC5";
                rev = "8eee4e3e6ce1d7075ec3eae67aa9780e67d161ed";
                sha256 = "CpGMyd6dCw4vW0yt5vUYbAjKLJ3zkZKWfowFUWOaCeg=";
                fetchSubmodules = true;
              };
            });
          };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).multimc-devel);
      };
}
