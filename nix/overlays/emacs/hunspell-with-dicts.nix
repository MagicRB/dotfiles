{ stdenv, appendToName
, hunspell, hunspellDicts
, makeWrapper
, lib

, dicts ? [ hunspellDicts.en_US ]
}:
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
}
