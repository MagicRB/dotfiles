{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, rlib, ... }@inputs:
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs { inherit system; };
            pkgs-unstable = import nixpkgs-unstable { inherit system; };

            pgtkEnable = true;
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

                  configureFlags = if pgtkEnable then (lib.remove "--with-xft" old.configureFlags)
                                                      ++ lib.singleton "--with-pgtk" else old.configureFlags; 

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
                    nodePackages.typescript-language-server nodePackages.typescript

                    pkgs-unstable.rust-analyzer
                    
                    (hunspell-with-dicts [ hunspellDicts.en_US ])
                    
                    w3m
                    sqlite
		                gcc

                    ghostscript
                    imagemagick
                    (texlive.combine { inherit (texlive) dvisvgm scheme-small preview siunitx amsmath ulem dvipng wrapfig cancel capt-of bytefield; }) # gensymb is not here, dont add
                    texlab

                    (rWrapper.override { packages = []; })
                  ]} --prefix EMACSLOADPATH : ${vtermModule}/lib:
                '';
              };

          };

        defaultPackage = rlib.forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).emacs);
      };
}
