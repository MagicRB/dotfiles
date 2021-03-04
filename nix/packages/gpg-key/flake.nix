{
  inputs = {
    # Not a flake
  };

  outputs = { self, nixpkgs, rlib, ... }@inputs:
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs { inherit system; };
          in
            with final;
            {
              gpg-key =
                let
                  gpg-key = rlib.substitute {
                    runCommand = pkgs.runCommandNoCCLocal;
                    name = "gpg-key";
                    inFile = ./gpg-key;
                    vars = {
                      cryptsetup = "${pkgs.cryptsetup}/bin/cryptsetup";
                      rm = "${pkgs.coreutils}/bin/rm";
                      ls = "${pkgs.coreutils}/bin/ls";
                      mkdir = "${pkgs.coreutils}/bin/mkdir";
                    };
                  };
                in
                  pkgs.writeShellScriptBin
                    "gpg-key"
                    (builtins.readFile "${gpg-key}");
                  
            };
              
        defaultPackage = rlib.forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).gpg-key);
      };
}
