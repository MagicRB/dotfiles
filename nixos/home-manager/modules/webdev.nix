{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, ... }:
{ config, lib, pkgs, ... }:
{
  home.packages = with nixpkgs; [
    wasm-pack
    nodePackages.vscode-html-languageserver-bin
    nodePackages.vscode-css-languageserver-bin
  ];
}
