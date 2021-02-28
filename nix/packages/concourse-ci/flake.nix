{
  inputs = {
    # Omitted, not a flake...
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      {
        overlay = system: final: prev:
          let
            pkgs = import nixpkgs { inherit system; };
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

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).concourse);
      };
}
