{
  inputs.nixpkgs.url = "git+https://github.com/NixOS/nixpkgs?ref=nixos-20.09";
  inputs.cura = {
    url = "git+https://github.com/Ultimaker/Cura?ref=4.8";
    flake = false;
  };
  inputs.materials = {
    url = "git+https://github.com/Ultimaker/fdm_materials?ref=4.8";
    flake = false;
  };

  outputs = { self, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: inputs.nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      rec {
        overlay = system: final: prev: let
          pkgs = inputs.nixpkgs.legacyPackages."${system}";
        in {
          cura = pkgs.cura;
          # with final; stdenv.mkDerivation {
          #   name = "cura";
          #   src = inputs.cura;

          #   buildInputs = with pkgs.qt5; [ qtbase qtquickcontrols2 qtgraphicaleffects ];
          #   propagatedBuildInputs = with pkgs.python37.pkgs; [
          #     libsavitar numpy-stl pyserial requests uranium zeroconf pynest2d
          #     sentry-sdk trimesh
          #   ]; # ++ plugins;
          #   nativeBuildInputs = with pkgs; [ cmake python37.pkgs.wrapPython qt5.wrapQtAppsHook ];

          #   cmakeFlags = [
          #     "-DURANIUM_DIR=${python37.pkgs.uranium.src}"
          #     "-DCURA_VERSION=4.8.0"
          #   ];

          #   makeWrapperArgs = [
          #     # hacky workaround for https://github.com/NixOS/nixpkgs/issues/59901
          #     "--set OMP_NUM_THREADS 1"
          #   ];

          #   postPatch = ''
          #     sed -i 's,/python''${PYTHON_VERSION_MAJOR}/dist-packages,/python''${PYTHON_VERSION_MAJOR}.''${PYTHON_VERSION_MINOR}/site-packages,g' CMakeLists.txt
          #     sed -i 's, executable_name = .*, executable_name = "${curaengine}/bin/CuraEngine",' plugins/CuraEngineBackend/CuraEngineBackend.py
          #   '';

          #   postInstall = ''
          #     mkdir -p $out/share/cura/resources/materials
          #     cp ${inputs.materials}/*.fdm_material $out/share/cura/resources/materials/
          #     mkdir -p $out/lib/cura/plugins
          #     for plugin in ${toString []}; do
          #       ln -s $plugin/lib/cura/plugins/* $out/lib/cura/plugins
          #     done
          #   '';

          #   postFixup = ''
          #     wrapPythonPrograms
          #     wrapQtApp $out/bin/cura
          #   '';
          # };
        };
        
        defaultPackage = forAllSystems (system: (import inputs.nixpkgs {
          inherit system;
          overlays = [ (self.overlay system) ];
        }).cura);
      };
}
