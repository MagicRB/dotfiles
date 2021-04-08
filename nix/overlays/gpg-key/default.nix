nglib:
final: prev:
{
  magic_rb.gpg-key = (nglib prev.stdenv.system).writeSubstitutedShellScriptBin {
    name = "gpg-key";
    file = ./gpg-key;
    substitutes = with prev; {
      inherit cryptsetup busybox findutils;
    };
  };
}
