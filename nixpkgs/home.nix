{ config, pkgs, ... }:

with import <nixpkgs> {};
with builtins;
with lib;

let
  dotfiles = ~/dotfiles;
  svt-av1 = let
    version = "0.8.5";
  in stdenv.mkDerivation rec {
    pname = "svt-av1";
    inherit version;

    nativeBuildInputs = [ cmake nasm ];

    src = fetchGit {
      url = "https://github.com/AOMediaCodec/SVT-AV1";
      ref = "v" + version;
    };
  };
  emacs = import ./emacs;
in {

  home.packages = with pkgs; [
    gcc
    gnumake
    pciutils

    patchelf

    steam

    zip

#    svt-av1
#    ffmpeg-svt-av1

    fira-code

    scrot
    feh

    multimc

    dunst
    i3status-rust
    picom
    font-awesome
    dejavu_fonts
    xclip

    rustup

#    blender
#    openscad

#    libreoffice
    discord

    qdirstat

    exa
    bat

    nix-du
    graphviz

#    graalvm8-ce
    
    sshfs
    mpv

    emacs.defaultPackage."x86_64-linux"
  ];

  home.file = {
    ".emacs".source = "${dotfiles}/.emacs";
    ".emacs.d/org" = {
      source = "${dotfiles}/.emacs.d/org";
      recursive = true;
    };
    ".emacs.d/lisp" = {
      source = "${dotfiles}/.emacs.d/lisp";
      recursive = true;
    };

    ".local/bin/emacsclient-remote" = {
      source = "${dotfiles}/.local/bin/emacsclient-remote";
      executable = true;
    };
    ".local/bin/screenshot" = {
      source = "${dotfiles}/.local/bin/screenshot";
      executable = true;
    };

    ".config/picom.conf".source = "${dotfiles}/.config/picom.conf";
    ".config/i3/config".text = (import "${dotfiles}/.config/i3/config.nix" { inherit config; inherit pkgs; }).config;
    ".config/i3/status.toml".text = (import "${dotfiles}/.config/i3/status.toml.nix" { inherit config; inherit pkgs; }).config;
    ".config/alacritty/alacritty.yaml".source = "${dotfiles}/.config/alacritty/alacritty.yml";
    ".config/dunst/dunstrc".source = "${dotfiles}/.config/dunst/dunstrc";

    ".bashrc".source = "${dotfiles}/.bashrc";
  };
}
