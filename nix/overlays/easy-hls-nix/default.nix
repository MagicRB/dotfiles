easy-hls-nix:
prev: final:
{
  magic_rb = prev.magic_rb or {} // {
    easy-hls-nix =
      prev.callPackage "${easy-hls-nix}/default.nix" {};
  };
}
