inputs: { lib, system }: {
  halfCallFlake = flakeSrc:
    let
      flake = import (flakeSrc + "/outputs.nix");
      outputs = flake ( inputs // { self = outputs; });
    in
      outputs;

  getLegacyPkgs =
    config:
    pkgs:
    lib.mapAttrs
      (_: value: import "${value}" { inherit system config; })
      pkgs;
}
