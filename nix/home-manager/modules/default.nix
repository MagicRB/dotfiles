{ ... }:
{
  imports = [
    ./alacritty
    ./bash
    ./ssh
    ./emacs
    # ./pulseaudio
    ./xmonad
    ./3d-printing.nix
    ./cmdline-utils.nix
    ./gpg.nix
    ./graphical-programs.nix
    ./multimc.nix
    ./pin-nixpkgs.nix
    ./optimisation.nix
    ./webdev.nix
    ./wine.nix
    ../../secret-lib
  ];
}
