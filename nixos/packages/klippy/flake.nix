{
  inputs = {
    nixpkgs.url = "nixpkgs";

    klippy = {
      url = "github:KevinOConnor/klipper?ref=master";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      {
        overlay = final: prev:
          with final; {
            klippy = let
              pythonPackages = python-packages: with python-packages; [
                greenlet
                cffi
                pyserial
                jinja2
              ]; 
              pythonWithPackages = (python2.withPackages pythonPackages);
            in stdenv.mkDerivation
              {
                name = "klippy";
                src = inputs.klippy;
                buildInputs = with pkgs; [ makeWrapper ];

                dontBuild = true;
                installPhase = ''
                  mkdir -p $out/share/klippy
                  cp -r klippy/* $out/share/klippy

                  mkdir -p $out/bin
                  chmod +x $out/share/klippy/klippy.py
                  makeWrapper \
                    $out/share/klippy/klippy.py \
                    $out/bin/klippy \
                    --prefix PATH : ${lib.makeBinPath (with pkgs; [
                      pythonWithPackages
                    ])}

                  ${pythonWithPackages}/bin/python $out/share/klippy/chelper/__init__.py
                '';              
              };
            
          };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).klippy);
      };
}
