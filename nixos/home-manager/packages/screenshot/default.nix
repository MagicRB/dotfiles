{ nixpkgs, nixpkgs-unstable, nixpkgs-master }:
(nixpkgs.writeShellScriptBin "screenshot" (builtins.readFile ./screenshot))
