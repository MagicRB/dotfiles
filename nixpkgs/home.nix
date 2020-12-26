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
  cura = import ./cura;
in {

  home.packages = with pkgs; [
    gcc
    gnumake
    pciutils

    patchelf

    steam

    rxvt-unicode
    zip
    tree
    rename
    pixz
    xorg.xinit

#    svt-av1
#    ffmpeg-svt-av1
    ffmpeg

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
    freecad
    openscad

#    libreoffice
    discord

    qdirstat

    exa
    bat

    (python38Full.withPackages (python-packages: with python-packages; [ jellyfin-apiclient-python ]))
    nix-du
    graphviz

#    graalvm8-ce
    
    sshfs
    mpv

    emacs.defaultPackage."x86_64-linux"
    cura.defaultPackage."x86_64-linux"
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
    ".local/bin/ogg-cover-art" = {
      source = "${dotfiles}/.local/bin/ogg-cover-art";
      executable = true;
    };
    ".local/bin/flac-to-ogg" = {
      source = "${dotfiles}/.local/bin/flac-to-ogg";
      executable = true;
    };

    ".config/picom.conf".source = "${dotfiles}/.config/picom.conf";
    ".config/i3/config".text = (import "${dotfiles}/.config/i3/config.nix" { inherit config; inherit pkgs; }).config;
    ".config/i3/status.toml".text = (import "${dotfiles}/.config/i3/status.toml.nix" { inherit config; inherit pkgs; }).config;
    ".config/alacritty/alacritty.yaml".source = "${dotfiles}/.config/alacritty/alacritty.yml";
    ".config/dunst/dunstrc".source = "${dotfiles}/.config/dunst/dunstrc";

    ".bashrc".source = "${dotfiles}/.bashrc";
    ".xinitrc".source = "${dotfiles}/.xinitrc";
  };
}
