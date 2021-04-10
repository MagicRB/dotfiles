nglib:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    gpg-key = (nglib prev.stdenv.system).writeSubstitutedShellScriptBin {
      name = "gpg-key";
      file = ./gpg-key;
      substitutes = with prev; {
        inherit cryptsetup busybox findutils;
      };
    };
  };
}
