{ bootloadHID, ... }:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    bootloadHID = prev.stdenv.mkDerivation {
      pname = "bootloadHID";
      version = "2012-12-08";

      src = bootloadHID;

      buildInputs = with prev;
        [ libusb-compat-0_1
        ];

      sourceRoot = "source/commandline";
      installPhase = ''
        install -d $out/bin
        install bootloadHID $out/bin
      '';
    };
  };
}
