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
              screenshot = pkgs.writeShellScriptBin "screenshot" (builtins.readFile ./screenshot);
            };

        defaultPackage = rlib.forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).screenshot);
      };
}
  
