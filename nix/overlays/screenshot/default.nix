nglib:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    screenshot = (nglib prev.stdenv.system).writeSubstitutedShellScriptBin {
      name = "screenshot"; 
      file = ./screenshot;
      substitutes = with prev; {
        inherit busybox scrot xclip;
      };
    };
  };
}
