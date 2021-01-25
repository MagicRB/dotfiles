inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, pkgs, ... }:
{
  home.packages = let
    nightly = custom.rust.nightly.latest.rust.override {
      extensions = [ "clippy" "rustfmt" ];
      targets = [ "x86_64-unknown-linux-gnu" "x86_64-unknown-linux-musl" ];
    };
  in [
    nightly
  ];
}
