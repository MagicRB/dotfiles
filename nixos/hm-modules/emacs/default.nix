inputs:
{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, rlib }:
{ config, lib, pkgs, ... }:
{
  home.packages = with custom; [
    emacs
  ] ++ (with nixpkgs; [
    fira-code
  ]);

  home.file = {
    ".emacs".source = ./.emacs;
    ".emacs.d/org" = {
      source = ./.emacs.d/org;
      recursive = true;
    };
    ".emacs.d/lisp" = {
      source = ./.emacs.d/lisp;
      recursive = true;
    };
  };
}
