{ home-manager,
  nixpkgs,
  inputs,
  pkgs,
  custom,
  self
}@ args:
let
  lib = nixpkgs.lib;
  nixosSystem = nixpkgs.lib.nixosSystem;
  homeManagerConfiguration = home-manager.lib.homeManagerConfiguration;

  callHalfFlake = flakeSrc:
    let
      flake = import (flakeSrc + "/flake.nix");
      outputs = flake.outputs ( inputs // { self = outputs; rlib = self; });
    in
      outputs;

  callHalfFlakes = flakes:
    lib.mapAttrs' (n: v: lib.nameValuePair n (callHalfFlake v)) flakes;

  moduleFlake = module: flake:
    (callHalfFlake flake).nixosModules."${module}";
  moduleFlakes = flakes:
    lib.mapAttrsToList (n: v: moduleFlake n v) flakes;

  loadNixpkgs = 
    { cross, system }:
    { pkgs, config }:

    (lib.mapAttrs
      (_: value:
        let
          pkgs = 
            import "${value}" {
              inherit config system;
            };
        in
          
          if cross == null then
            pkgs
          else
            pkgs.pkgsCross."${cross}"
      ) pkgs);

  loadCustom =
    system:
    pkg:
    if builtins.isAttrs pkg then
      if builtins.hasAttr "defaultPackage" pkg then
        pkg.defaultPackage."${system}"
      else
        pkg
    else if builtins.isFunction pkg then
      pkg system          
    else if builtins.isPath pkg then
      import pkg
    else
      builtins.throw "Invalid custom package!";

  loadCustoms =
    system:
    pkgs:
    lib.mapAttrs (n: v: loadCustom system v) pkgs;

  callModule =
    { pkgs, custom, hostname }:
    module:

    args:
    let
      fetchedModule =
        (if builtins.isFunction module then
          module
         else
           import module);
      sanitizedArgs = builtins.removeAttrs args [ "pkgs" ];

      calledModule = fetchedModule
        (pkgs // { inherit hostname custom; rlib = self; })
        sanitizedArgs;

      recursedModule =
        calledModule //
        {
          imports =
            (if builtins.hasAttr "imports" calledModule then
              (callModules stuff calledModule.imports)
             else
               [])
            ++
            (if builtins.hasAttr "compatImports" calledModule then
              (callCompatModules stuff calledModule.compatImports)
             else
               []);
        };

      stuff = { inherit pkgs custom hostname; };
    in
      builtins.removeAttrs recursedModule [ "compatImports" ];

  callModules =
    { pkgs, custom, hostname }:
    modules:

    builtins.map (module:
      callModule { inherit pkgs hostname custom; } module
    ) modules;

  callCompatModule =
    module:

    let
      fetchedModule =
        (if builtins.isFunction module then
          module
         else if builtins.isPath module then
           if lib.pathIsRegularFile module then
             import module
           else
             builtins.throw "CompatModule path is not a file."
         else
           builtins.throw "CompatModule is neither a path nor a function."
        );
    in
      fetchedModule;# (args // { pkgs = pkgs.nixpkgs; })

  loadHomeConfiguration =
    { configuration, pkgs, custom, hostname }:
    lib.mapAttrs
      (n: v: callModule {
        inherit hostname pkgs custom;
      } v) configuration;

  callCompatModules =
    modules:

    builtins.map (module:
      callCompatModule module
    ) modules;


  substitute = { runCommand, name, inFile, vars }: 
    runCommand name {}
      (let
        varsStr = lib.mapAttrsToList
          (name: value: ''--subst-var-by "${name}" "${value}"'')
          vars;
      in
        ''
          substitute ${inFile} $out \
          ${builtins.concatStringsSep " " varsStr}
        '');

in {
  inherit callHalfFlake callHalfFlakes moduleFlakes callCompatModules substitute;

  nixosSystem =
    { cross ? null,
      system,
      config ? {},
      modules ? [],
      compatModules ? [],
      hm ? null,
      hostname,
      check ? true
    }:
    let
      loadedPkgs =
        loadNixpkgs
          { inherit cross system; }
          { inherit pkgs config; };
      loadedCustoms =
        loadCustoms system custom;
    in nixosSystem {
      inherit check system;

      # pkgs = loadedPkgs.nixpkgs;

      modules = (callModules {
        inherit hostname;
        pkgs = loadedPkgs;
        custom = loadedCustoms;
      } modules) ++ (callCompatModules compatModules)
      ++
      [({ ... }: {
        nixpkgs.config = config;
      })]
      ++
      (if hm != null then
        [ home-manager.nixosModules.home-manager
          ({ config, ... }:
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;

              home-manager.users = loadHomeConfiguration
                { pkgs = loadedPkgs;
                  custom = loadedCustoms;
                  inherit hostname;
                  configuration = hm;
                };
            })
        ]
       else
         []);
    };
  homeManagerConfiguration =
    { cross ? null,
      system,
      username,
      homeDirectory,
      config ? {},
      modules ? [],
      compatModules ? [],
      hostname
    }:
    let
      loadedPkgs =
        loadNixpkgs
          { inherit cross system; }
          { inherit pkgs config; };
      loadedCustoms =
        loadCustoms system custom;
    in homeManagerConfiguration {
      inherit system username homeDirectory;

      pkgs = loadedPkgs.nixpkgs;

      configuration = _: {
        imports = (callModules {
          inherit hostname;
          pkgs = loadedPkgs;
          custom = loadedCustoms;
        } modules)
        ++ (callCompatModules compatModules);
      };
    };
}
