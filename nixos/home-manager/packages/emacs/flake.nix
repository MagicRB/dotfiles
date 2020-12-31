{
  inputs = {
    nixpkgs.url = "nixpkgs";
    nixpkgs-unstable.url = "nixpkgs-unstable";

    emacs-overlay.url = "git+https://github.com/nix-community/emacs-overlay";
    emacs = {
      type = "git";
      url = "https://git.savannah.gnu.org/git/emacs.git";
      ref = "feature/native-comp";
      flake = false;
    };
    vtermModule = {
      url = "git+https://github.com/akermu/emacs-libvterm";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs { inherit system; };
            pkgs-unstable = import nixpkgs-unstable { inherit system; };
          in
          with final; {
            emacs = let
              name = "emacs";
              version = "28";

              emacs-overlay = inputs.emacs-overlay.overlay pkgs pkgs;
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
            in with pkgs;
              stdenv.mkDerivation {
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

                    pkgs-unstable.rust-analyzer
                    
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
              };

          };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).emacs);
      };
}
