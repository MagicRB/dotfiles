inputs:
final: prev:
let
  hunspellWithDicts = prev.callPackage ./hunspell-with-dicts.nix {
    dicts = with prev.hunspellDicts; [ en_US ];
  };
in
{
  magic_rb = {
    emacs = prev.callPackage ./emacs-bundle.nix rec {
      emacsOverlay = inputs.emacs-overlay.overlay prev prev;
      emacsSrc = inputs.emacs;
      vtermModule = inputs.vtermModule;

      emacsPackages = with prev; 
        [
          nodePackages.pyright
          python38Full
          nodePackages.typescript-language-server nodePackages.typescript

          rust-analyzer
          
          hunspellWithDicts
          
          w3m
          sqlite
          gcc

          ghostscript
          imagemagick
          (texlive.combine { inherit (texlive) dvisvgm scheme-small preview siunitx amsmath ulem dvipng wrapfig cancel capt-of bytefield; }) # gensymb is not here, dont add
          texlab

          (rWrapper.override { packages = []; })
        ];
    };
  };
}
