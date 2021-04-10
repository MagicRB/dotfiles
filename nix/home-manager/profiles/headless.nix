{ pkgs, ... }: {
  home.packages = with pkgs; [
    nixFlakes
  ];

  imports = [
    ../modules/pin-nixpkgs.nix
    ../modules/bash
  ];
}
