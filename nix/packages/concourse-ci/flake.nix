{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs-unstable, rlib, concourse, custom, ... }@inputs:
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs-unstable { inherit system; };
          in
            rec {
              concourse = prev.fly.overrideAttrs (old: {
                pname = "concourse";
                subPackages = [ "cmd/concourse" ];

                buildFlagsArray = ''
                  -ldflags=
                    -X github.com/concourse/concourse.Version=${old.version}
                '';
                postInstall = null;
              });

              concourse-static =
                let
                  yarn2nix = custom.yarn2nix.defaultPackage."${system}";
                  npm-deps = prev.stdenv.mkDerivation {
                    name = "npm-deps.nix";
                    src = inputs.concourse;

                    patches = [ ./0001-patch-git-package-in-yarn-lock.diff ];

                    buildPhase = ''
                      ${yarn2nix}/bin/yarn2nix --offline yarn.lock > npm-deps.nix
                    '';
                    installPhase = ''
                      cp npm-deps.nix $out
                    '';
                  };
                  npm-package = prev.stdenv.mkDerivation {
                    name = "npm-package.nix";
                    src = inputs.concourse;

                    patches = [ ./0001-patch-git-package-in-yarn-lock.diff ];

                    buildPhase = ''
                      ${yarn2nix}/bin/yarn2nix --offline --template package.json > npm-package.nix
                    '';
                    installPhase = ''
                      cp npm-package.nix $out
                    '';
                  };
                  nixLib = (import inputs.yarn2nix { pkgs = prev; }).nixLib;
                  node_modules = nixLib.buildNodePackage
                    ( { src = inputs.concourse; } //
                      nixLib.callTemplate npm-package
                        (nixLib.buildNodeDeps (nixpkgs-unstable.legacyPackages."${system}".callPackage npm-deps {})));
                in
                  prev.stdenv.mkDerivation {
                    pname = "concourse-static";
                    version = concourse.version;

                    src = node_modules;
                    nativeBuildInputs = [ prev.yarn prev.nodejs ];

                    buildPhase = ''
                      yarn run webpack-cli --mode production 
                    '';
                  };
            };

        defaultPackage = rlib.forAllSystems (system: (import nixpkgs-unstable {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).concourse);
      };
}
