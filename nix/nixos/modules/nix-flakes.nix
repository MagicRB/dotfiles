{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, ... }:
{
  nix = let
    nix = nixpkgs-master.nixFlakes;
  in {
    package = nix; 
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';
  };
}
