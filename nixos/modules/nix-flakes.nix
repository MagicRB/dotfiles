{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
{
  nix = let
    nix = nixpkgs.nixFlakes;
  in {
    package = nix; 
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
