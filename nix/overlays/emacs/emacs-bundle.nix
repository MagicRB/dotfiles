{
  makeWrapper 
, stdenv, callPackage, lib

, emacsBaseOverrides ? {}
, vtermModuleOverrides ? {}

, emacsOverlay
, emacsSrc
, vtermModule

, emacsPackages ? []

, libgccjit
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
    makeWrapper ${emacsBase}/bin/emacs $out/bin/emacs \
	--prefix PATH : ${lib.makeBinPath emacsPackages} \
	--prefix EMACSLOADPATH : ${vterm}/lib: \
	--prefix LIBRARY_PATH : ${lib.makeLibraryPath [ stdenv.cc.cc stdenv.glibc ]}:${lib.getLib libgccjit + /lib/gcc/x86_64-unknown-linux-gnu/9.3.0}
  '';
}
