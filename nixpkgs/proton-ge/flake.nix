{
  inputs.nixpkgs.url = "nixpkgs";
  inputs.proton-ge = {
    url = "git+https://github.com/GloriousEggroll/proton-ge-custom";
    flake = false;
  };

  outputs = { self, nixpkgs, ... }@inputs: {
    proton-ge = nixpkgs.pkgs.stdenv.mkDerivation {
      src = inputs.proton-ge;
    };

    defaultPackage = let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: inputs.nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      forAllSystems (system: (import inputs.nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      }).emacsNativeComp);
  };
}
