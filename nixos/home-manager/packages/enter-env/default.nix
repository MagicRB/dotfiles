{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, ... }:
let
  writeShellScriptBin = nixpkgs.writeShellScriptBin;
  prusa-slicer = nixpkgs-unstable.callPackage ./prusa-slicer.nix {};
in
writeShellScriptBin "enter-env" ''
  NIX_SHELL_PRESERVE_PROMPT=1 nix-shell -p ${nixpkgs.openscad} ${nixpkgs.cura} ${prusa-slicer} ${nixpkgs.inkscape}
''
