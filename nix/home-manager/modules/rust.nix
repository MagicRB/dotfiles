{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, ... }:
{
  home.packages = let
    nightly = custom.rust.nightly.latest.rust.override {
      extensions = [ "clippy" ];
      targets = [ "x86_64-unknown-linux-gnu" "x86_64-unknown-linux-musl" ];
    };
  in [
    nightly
  ];
}
