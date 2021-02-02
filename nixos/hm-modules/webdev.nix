{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib,  ... }:
{
  home.packages = with nixpkgs; [
    wasm-pack
    nodePackages.vscode-html-languageserver-bin
    nodePackages.vscode-css-languageserver-bin
  ];
}
