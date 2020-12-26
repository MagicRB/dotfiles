{ nixpkgs, nixpkgs-unstable, nixpkgs-master }:
(nixpkgs.writeShellScriptBin "emacsclient-remote" (builtins.readFile ./emacsclient-remote))
