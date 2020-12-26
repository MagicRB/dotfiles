inputs: { config, pkgs, ... }:

{
  nix.registry."nixpkgs".flake = inputs.nixpkgs;
  nix.registry."nixpkgs-unstable".flake = inputs.nixpkgs-unstable;
  nix.registry."nixpkgs-master".flake = inputs.nixpkgs-master;
}
