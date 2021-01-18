inputs: { lib, system }: {
  halfCallFlake = flakeSrc:
    let
      flake = import (flakeSrc + "/flake.nix");
      outputs = flake.outputs ( inputs // { self = outputs; });
    in
      outputs.defaultPackage."${system}";

  getLegacyPkgs =
    config:
    pkgs:
    lib.mapAttrs
      (_: value: import "${value}" { inherit system config; })
      pkgs;
}
