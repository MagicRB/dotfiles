inputs:
final: prev:
let
  hunspellWithDicts = prev.callPackage ./hunspell-with-dicts.nix {
    dicts = with prev.hunspellDicts; [ en_US ];
  };
  nixpkgs-unstable = import inputs.nixpkgs-unstable
    { system = prev.stdenv.system; };
in
{
  magic_rb = prev.magic_rb or {} // {
    emacs = prev.callPackage ./emacs-bundle.nix rec {
      emacsOverlay = inputs.emacs-overlay.overlay prev prev;
      emacsSrc = inputs.emacs;
      vtermModule = inputs.vtermModule;

      emacsPackages = with nixpkgs-unstable; 
        [
          hunspellWithDicts
          
          sqlite
          gcc

          ghostscript
          imagemagick
          (texlive.combine { inherit (texlive) dvisvgm scheme-small preview siunitx amsmath ulem dvipng wrapfig cancel capt-of bytefield chemfig simplekv; }) # gensymb is not here, dont add
          texlab

          (rWrapper.override { packages = []; })

          gnumake
          clang-tools

          rnix-lsp
        ];
    };
  };
}
