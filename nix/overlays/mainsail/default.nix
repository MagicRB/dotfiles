nglib:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    mainsail = final.callPackage ./mainsail.nix {};
  };
}
