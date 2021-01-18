{
  inputs = {
    nixpkgs.url = "nixpkgs";
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
            prusa-slicer = nixpkgs-unstable.callPackage ./prusa-slicer.nix {};
          in {
            enter-env = writeShellScriptBin "enter-env" ''
              NIX_SHELL_PRESERVE_PROMPT=1 nix-shell -p ${nixpkgs.openscad} ${nixpkgs.cura} ${prusa-slicer} ${nixpkgs.inkscape}
            '';
          };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).enter-env);
      };
}
