{ home-manager,
  nixpkgs,
  inputs,
  pkgs,
  custom,
  supportedSystems,
  self
}@ args:
let
  lib = nixpkgs.lib;
  nixosSystem = nixpkgs.lib.nixosSystem;
  homeManagerConfiguration = home-manager.lib.homeManagerConfiguration;

  callHalfFlake = { custom-self, flakeSrc }:
    if builtins.isAttrs flakeSrc then
      flakeSrc
    else if builtins.isPath flakeSrc then
      let
        flake = import (flakeSrc + "/flake.nix");
        outputs =  flake.outputs ( inputs // { self = outputs; rlib = self; custom = custom-self; });
      in
        outputs
    else
      builtins.throw "Invalid custom package!";

  callHalfFlakes = { custom-self, flakes }:
    lib.mapAttrs' (name: flakeSrc: lib.nameValuePair name (callHalfFlake { inherit custom-self flakeSrc; })) flakes;

  moduleFlake = { custom-self, module, flake }:
    (callHalfFlake custom-self flake).nixosModules."${module}";
  moduleFlakes = { custom-self, flakes }:
    lib.mapAttrsToList (module: flake: moduleFlake { inherit custom-self module flake; }) flakes;

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

  callDocker =
    { pkgs, custom }:
    docker: 
    let
      fetchedModule =
        (if builtins.isFunction docker then
          docker
         else
           import docker);
    in
      (fetchedModule (pkgs // { inherit custom; rlib = self; }));

  forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

  parsedCustom = callHalfFlakes { custom-self = parsedCustom; flakes = custom; };
in {
  inherit callHalfFlake callHalfFlakes moduleFlakes callCompatModules substitute forAllSystems;

  custom = parsedCustom;

  dockerImages =
    images:
    forAllSystems (system: 
      lib.mapAttrs
        (_: value:
          let
            loadedPkgs =
              loadNixpkgs
                { inherit system; cross = null; }  # TODO really disable cross?
                { inherit pkgs; config = {}; };  # TODO handle config
            loadedCustoms =
              loadCustoms system (callHalfFlakes { custom-self = loadedCustoms; flakes = custom; }); # TODO handle config
          in 
            callDocker {
              pkgs = loadedPkgs;
              custom = loadedCustoms;
            } value) images);

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
        loadCustoms system (callHalfFlakes { custom-self = loadedCustoms; flakes = custom; });
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
        loadCustoms system (callHalfFlakes { custom-self = loadedCustoms; flakes = custom; });
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

  linkFarm = name: entries:
    forAllSystems (system: (import pkgs.nixpkgs { inherit system; }).linkFarm name (entries system));
}
