{ nixpkgs, nixpkgs-unstable, nixpkgs-master }:
inputs:
let
  name = "emacs";
  version = "28";

  emacs-overlay = inputs.emacs-overlay.overlay nixpkgs nixpkgs;
  hunspell-with-dicts = with nixpkgs; dicts:
    let
      searchPath = lib.makeSearchPath "share/hunspell" dicts;
    in	
      stdenv.mkDerivation {
        name = (appendToName "with-dicts" hunspell).name;
        buildInputs = [ makeWrapper ];
        buildCommand = ''
              makeWrapper ${hunspell.bin}/bin/hunspell $out/bin/hunspell --prefix DICPATH : ${searchPath}
            '';
        meta = removeAttrs hunspell.meta ["outputsToInstall"];
      };
  emacs = with nixpkgs; with emacs-overlay; ((emacsGit.overrideAttrs (
    old: {
      src = inputs.emacs;
      buildInputs = old.buildInputs ++ [ jansson harfbuzz.dev glib-networking ];
      makeFlags = [ "NATIVE_FULL_AOT=1" ];
      inherit name version;
    }
  )).override { nativeComp = true; });
  vtermModule = with nixpkgs; stdenv.mkDerivation {
    name = "vtermModule";
    src = inputs.vtermModule;
    buildInputs = [ cmake libtool glib.dev libvterm-neovim ];
    cmakeFlags = [
      "-DEMACS_SOURCE=${emacs.src}"
      "-DUSE_SYSTEM_LIBVTERM=ON"
    ];
    installPhase = ''
      mkdir -p $out/lib
      install ../vterm-module.so $out/lib
    '';
  };
in
with nixpkgs; stdenv.mkDerivation {
  name = "emacs";
  buildInputs = [ makeWrapper emacs ];
  unpackPhase = "true";
  buildPhase = "true";
  installPhase = ''
    mkdir -p $out/bin
    ln -s ${emacs}/bin/emacsclient $out/bin/emacsclient
    makeWrapper ${emacs}/bin/emacs $out/bin/emacs --prefix PATH : ${lib.makeBinPath [
      nodePackages.pyright
      python38Full
      nodePackages.typescript-language-server

      fira-code

      rust-analyzer
      
      (hunspell-with-dicts [ hunspellDicts.en_US ])
                
      w3m
      sqlite

      webkitgtk

      ghostscript
      imagemagick
      (texlive.combine { inherit (texlive) scheme-small siunitx amsmath ulem dvipng wrapfig cancel capt-of; })
      texlab

      gcc
    ]} --prefix EMACSLOADPATH : ${vtermModule}/lib:
  '';
}
