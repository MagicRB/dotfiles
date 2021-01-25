inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rust-bin, rlib }@ rpkgs:
{ config, lib, pkgs, ... }: {
  service.gpg-agent.enable = true;
}
