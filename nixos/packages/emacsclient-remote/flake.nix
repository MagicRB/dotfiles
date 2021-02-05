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
          let
            pkgs = import nixpkgs { inherit system; };
          in
            with final; {
              emacsclient-remote = pkgs.writeShellScriptBin "emacsclient-remote" (builtins.readFile ./emacsclient-remote);
            };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).emacsclient-remote);
      };
}
  
