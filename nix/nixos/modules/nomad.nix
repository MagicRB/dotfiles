{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
opts:
let
  nomadModule = import "${inputs.nixpkgs-unstable}/nixos/modules/services/networking/nomad.nix";
in
nomadModule (opts // { pkgs = nixpkgs; })
