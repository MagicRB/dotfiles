{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs-unstable, rlib, ... }@inputs:
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs-unstable { inherit system; };
          in
            {
              concourse = prev.fly.overrideAttrs (old: {
                pname = "concourse";
                subPackages = [ "cmd/concourse" ];

                buildFlagsArray = ''
                  -ldflags=
                    -X github.com/concourse/concourse.Version=${old.version}
                '';
                postInstall = null;
              });
            };

        defaultPackage = rlib.forAllSystems (system: (import nixpkgs-unstable {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).concourse);
      };
}
