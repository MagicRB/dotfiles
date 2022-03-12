{ hidapitester, ... }:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    hidapitester = prev.stdenv.mkDerivation {
      pname = "hidapitester";
      version = "0.2";

      buildInputs = with prev;
        [ libudev
        ];

      nativeBuildInputs = with prev;
        [ pkg-config 
        ];

      unpackPhase = ''
        runHook preUnpack

        install -d /build/source/hidapi
        install -d /build/source/hidapitester

        cp -r ${prev.hidapi.src}/. /build/source/hidapi
        cp -r ${hidapitester}/. /build/source/hidapitester

        chmod -R 755 /build/source

        runHook postUnpack
      '';

      HIDAPI_DIR = "/build/source/hidapi";
      sourceRoot = "/build/source/hidapitester";

      installPhase = ''
        install -d $out/bin
        install hidapitester $out/bin
      '';
    };
  };
}
