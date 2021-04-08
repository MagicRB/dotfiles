{ jansson, harfbuzz, glib-networking
, lib

, withPgtk ? true
, withNativeComp ? true
, name ? "emacs"
, emacsOverlay
, emacsSrc
}:
(emacsOverlay.emacsGit.overrideAttrs
  (old: {
    src = emacsSrc;
    buildInputs = 
      old.buildInputs ++ [ jansson harfbuzz.dev glib-networking ];
    makeFlags = [ "NATIVE_FULL_AOT=1" ];

    configureFlags =
      if withPgtk then
        (lib.remove "--with-xft" old.configureFlags)
        ++ lib.singleton "--with-pgtk"
      else
        old.configureFlags; 

    inherit name;
  })).override { nativeComp = withNativeComp; }
