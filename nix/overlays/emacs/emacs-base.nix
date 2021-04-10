{ jansson, harfbuzz, glib-networking
, lib

, withPgtk ? true
, withNativeComp ? true
, name ? "emacs"
, emacsOverlay
, emacsSrc
}:
with lib;
(emacsOverlay.emacsGit.overrideAttrs
  (old: {
    src = emacsSrc;
    buildInputs = 
      old.buildInputs ++ [ jansson harfbuzz.dev glib-networking ];
    makeFlags = [ "NATIVE_FULL_AOT=1" ];

    configureFlags =
      (if withPgtk then
        (remove "--with-xft" old.configureFlags)
        ++ singleton "--with-pgtk"
      else
        old.configureFlags) ++ (optional withNativeComp "--with-native-compilation"); 

    inherit name;
  })).override { nativeComp = withNativeComp; }
