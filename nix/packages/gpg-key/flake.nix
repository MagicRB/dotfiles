{
  inputs = {
    # Not a flake
  };

  outputs = { self, nixpkgs, rlib, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs { inherit system; };
          in
            with final;
            let
              gpg-key = pkgs.writeShellScriptBin
                "gpg-key"
                (builtins.readFile ./gpg-key);
            in
              rlib.substitute {
                runCommand = pkgs.runCommandNoCCLocal;
                name = "gpg-key";
                inFile = "${gpg-key}";
                vars = {
                  cryptsetup = "${pkgs.cryptsetup}/bin/cryptsetup";
                  rm = "${pkgs.coreutils}/bin/rm";
                  ls = "${pkgs.coreutils}/bin/ls";
                  mkdir = "${pkgs.coreutils}/bin/mkdir";
                  mount = "${pkgs.coreutils}/bin/mount";
                  umount = "${pkgs.coreutils}/bin/umount";
                };
              };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).gpg-key);
      };
}
