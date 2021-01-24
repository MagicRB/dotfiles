inputs: { lib, system }: rec {
  halfCallFlake = flakeSrc:
    let
      flake = import (flakeSrc + "/flake.nix");
      outputs = flake.outputs ( inputs // { self = outputs; });
    in
      outputs;

  halfCallFlakePackage = flakeSrc:
    (halfCallFlake flakeSrc).defaultPackage."${system}";

  callModule = pkgs: module:
    (import module inputs pkgs);

  callModules = pkgs: modules:
    builtins.map (module: (callModule pkgs module)) modules;
    
  getLegacyPkgs =
    config:
    pkgs:
    lib.mapAttrs
      (_: value: import "${value}" { inherit system config; })
      pkgs;
  substitute = runCommand: name: inFile: vars: 
    runCommand name {}
      (let
        varsStr = lib.mapAttrsToList (name: value: ''--subst-var-by "${name}" "${value}"'') vars;
      in
        ''
          substitute ${inFile} $out \
          ${builtins.concatStringsSep " " varsStr}
        '');
  binPath = drv: name:
    "${drv}/bin/${name}";
}
