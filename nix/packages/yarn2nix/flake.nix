{
  inputs = {
    # not a flake
  };

  outputs = { self, nixpkgs-unstable, rlib, yarn2nix, ... }: {
    overlay = system: final: prev:
      let
        pkgs = import nixpkgs-unstable { inherit system; };
        tmp = import "${yarn2nix}/default.nix" { inherit pkgs; };
      in
        {
          yarn2nix = tmp;
        };
    

    defaultPackage = rlib.forAllSystems (system: (import nixpkgs-unstable {
      inherit system;
      overlays = [ (self.overlay system) ];
    }).yarn2nix);
  };
}
