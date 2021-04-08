{ cmake, libtool, glib, libvterm-neovim
, stdenv

, vtermModule
, emacsBase }:
stdenv.mkDerivation {
  name = "vterm-module";
  src = vtermModule;
  buildInputs = [ cmake libtool glib.dev libvterm-neovim ];
  cmakeFlags = [
    "-DEMACS_SOURCE=${emacsBase.src}"
    "-DUSE_SYSTEM_LIBVTERM=ON"
  ];
  installPhase = ''
      mkdir -p $out/lib
      install ../vterm-module.so $out/lib
    '';
}
