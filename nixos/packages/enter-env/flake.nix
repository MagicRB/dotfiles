{
  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, ... }@inputs:
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
            prusa-slicer = (import nixpkgs-unstable { inherit system; }).callPackage ./prusa-slicer.nix {};
          in {
            enter-env = writeShellScriptBin "enter-env" ''
              NIX_SHELL_PRESERVE_PROMPT=1 nix shell ${pkgs.openscad} ${pkgs.cura} ${prusa-slicer} ${pkgs.inkscape}
            '';
          };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).enter-env);
      };
}
