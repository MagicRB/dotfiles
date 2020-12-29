{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom }:
let
  writeShellScriptBin = nixpkgs.writeShellScriptBin;
in
writeShellScriptBin "enter-env" ''
  NIX_SHELL_PRESERVE_PROMPT=1 nix-shell -p ${nixpkgs.openscad} ${nixpkgs.cura}
''
