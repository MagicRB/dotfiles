inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, pkgs, ... }:
{
  nix = let
    nix = pkgs.nixFlakes;
  in {
    package = nix; 
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
