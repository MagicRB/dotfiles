easy-hls-nix:
final: prev:
{
  magic_rb = prev.magic_rb or {} // {
    easy-hls-nix =
      prev.callPackage "${easy-hls-nix}/default.nix" {};
  };
}
