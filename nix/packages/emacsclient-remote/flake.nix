{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs, rlib, ... }@inputs:
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs { inherit system; };
          in
            with final; {
              emacsclient-remote = pkgs.writeShellScriptBin "emacsclient-remote" (builtins.readFile ./emacsclient-remote);
            };

        defaultPackage = rlib.forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).emacsclient-remote);
      };
}
  
