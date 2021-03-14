{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = (if hostname != "edge" then with custom; [
    emacs
  ] else with nixpkgs; [ emacs ]) ++ (with nixpkgs; [
    fira-code
    emacs-all-the-icons-fonts
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
