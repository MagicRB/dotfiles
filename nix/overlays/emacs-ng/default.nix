{ emacs, vtermModule, nixpkgs-unstable, ... }:
final: prev:
let
  inherit (prev) lib callPackage stdenv;
in
{
  magic_rb = prev.magic_rb or {} // {
    libvterm-emacs = stdenv.mkDerivation {
      name = "vterm-emacs";
      src = vtermModule;
      buildInputs = with prev; [ cmake libtool glib.dev libvterm-neovim ];
      cmakeFlags = [
        "-DEMACS_SOURCE=${emacs}"
        "-DUSE_SYSTEM_LIBVTERM=ON"
      ];
      installPhase = ''
        mkdir -p $out/lib
        install ../vterm-module.so $out/lib
      '';
    };

    emacs = callPackage
      ({ giflib
       , librsvg
       , glib-networking
       , webkitgtk
       , xorg, libXpm ? xorg.libXpm
       , magic_rb, libvterm-emacs ? magic_rb.libvterm-emacs
       , makeWrapper
       , lndir

       , march ? "znver2"
       , additionalPackages ?
         (with prev;
           [ (symlinkJoin { name = "hledger-compat"; paths = [ hledger ]; postBuild = "ln -s $(readlink -f $out/bin/hledger) $out/bin/ledger"; })
         ])
       }:
         let
           base = (callPackage (import "${nixpkgs-unstable}/pkgs/applications/editors/emacs/generic.nix"
             {
               version = "28";
               sha256 = lib.fakeSha256;
               patches = _: [];
             })
             {
               withX = false;
               withGTK3 = false;
               nativeComp = true;
               srcRepo = true;

               libXaw = null;
               Xaw3d = null;
               gconf = null;
               alsa-lib = null;
               acl = null;
               gpm = null;

               AppKit = null;
               GSS = null;
               ImageIO = null;
               sigtool = null;
             }).overrideAttrs
             (old:
               {
                 src = emacs;

                 configureFlags =
                   [ "--disable-build-details"
                     "--with-native-compilation"

                     "--with-xpm=yes" "--with-jpeg=yes" "--with-png=yes" "--with-gif=yes" "--with-tiff=yes"
                     "--with-json=yes"
                     "--with-cairo" "--without-x" "--with-pgtk"
                     "--with-modules"
                     "--prefix=\${out}"
                   ];

                 NIX_CFLAGS_COMPILE="-O2 -pipe -march=${march}";

                 buildInputs =
                   old.buildInputs
                   ++ [ giflib librsvg glib-networking webkitgtk xorg.libXpm  ];

                 makeFlags = old.makeFlags or [] ++
                             [ "NATIVE_FULL_AOT=1"
                             ];
               });
        in
          stdenv.mkDerivation {
            inherit (base) pname version;

            phases = [ "buildPhase" ];

            nativeBuildInputs = [ makeWrapper ];

            buildPhase = ''
              mkdir -p $out
              ${lndir}/bin/lndir -silent ${base} $out
              wrapProgram $out/bin/emacs \
                --prefix EMACSLOADPATH : ${libvterm-emacs}/lib: \
                --prefix PATH : ${lib.makeBinPath additionalPackages}
            '';
          })
      {};
  };
}
