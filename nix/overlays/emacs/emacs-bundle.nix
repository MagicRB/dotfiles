{
  makeWrapper 
, stdenv, callPackage, lib

, emacsBaseOverrides ? {}
, vtermModuleOverrides ? {}

, emacsOverlay
, emacsSrc
, vtermModule

, emacsPackages ? []
}:
let
  emacsBase = callPackage ./emacs-base.nix rec {
    withPgtk = true;
    withNativeComp = true;
    name = "emacs";
    inherit emacsOverlay emacsSrc;
  } // emacsBaseOverrides;
  vterm = callPackage ./vterm-module.nix {
    inherit emacsBase vtermModule;
  } // vtermModuleOverrides;
in
stdenv.mkDerivation {
  name = "emacs";
  buildInputs = [ makeWrapper ];
  unpackPhase = "true";
  buildPhase = "true";
  installPhase = ''
    mkdir -p $out/bin
    ln -s ${emacsBase}/bin/emacsclient $out/bin/emacsclient
    makeWrapper ${emacsBase}/bin/emacs $out/bin/emacs --prefix PATH : ${lib.makeBinPath emacsPackages} --prefix EMACSLOADPATH : ${vterm}/lib:
  '';
}
