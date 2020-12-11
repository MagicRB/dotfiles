{
  description = "Emacs tracking git, with native compilation and maybe pgkt";

  inputs.emacs-overlay.url = "git+https://github.com/nix-community/emacs-overlay";
  inputs.nixpkgs.url = "nixpkgs";
  inputs.emacs = {
    type = "git";
    url = "https://git.savannah.gnu.org/git/emacs.git";
    ref = "feature/native-comp";
    flake = false;
  };
  inputs.vtermModule = {
    url = "git+https://github.com/akermu/emacs-libvterm";
    flake = false;
  };

  outputs = { self, ... }@inputs:
    let
      name = "emacs";
      version = "28";

      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: inputs.nixpkgs.lib.genAttrs supportedSystems (system: f system);

      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
      emacs-overlay = inputs.emacs-overlay.overlay pkgs inputs.nixpkgs;
      hunspell-with-dicts = with pkgs; dicts:
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
      emacs = with pkgs; with emacs-overlay; ((emacsGit.overrideAttrs (
        old: {
          src = inputs.emacs;
          buildInputs = old.buildInputs ++ [ jansson harfbuzz.dev glib-networking ];
          inherit name version;
        }
      )).override { nativeComp = true; });
      vtermModule = final: with final; stdenv.mkDerivation {
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
      rec {
        overlay = final: prev: {
          emacsNativeComp = with final; stdenv.mkDerivation {
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
              ]} --prefix EMACSLOADPATH : ${vtermModule final}/lib:
            '';
          };
        };

        defaultPackage = forAllSystems (system: (import inputs.nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).emacsNativeComp);
      };
}
