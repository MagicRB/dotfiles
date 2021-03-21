{
  inputs = {
    # ...
  };

  outputs = { self, nixpkgs, rlib, ... }:
    {
      overlay = system: final: prev:
        let
          pkgs = import nixpkgs { inherit system; };
          freecad-src = pkgs.fetchurl {
            url = "https://github.com/FreeCAD/FreeCAD/releases/download/0.19_pre/FreeCAD_0.19-24267-Linux-Conda_glibc2.12-x86_64.AppImage";
            sha256 = "Nwlkd8imHGJl9Nwf56PCH8V/g8EZX7wcNsa/niub5Pk=";
          }; 
          python3 = pkgs.python3.withPackages (pypkgs: with pypkgs; [
            GitPython
            numpy
          ]);
        in
          with final; {
            freecad-appimage =
              pkgs.writeShellScriptBin "freecad" ''
                export APPIMAGE_DEBUG_EXEC=usr/bin/freecad \
                        LD_LIBRARY_PATH=${pkgs.pciutils}/lib # PATH=${python3}/bin:${pkgs.git}/bin
                ${pkgs.appimage-run}/bin/appimage-run ${freecad-src}
              '';
          };
      defaultPackage."x86_64-linux" =
        let
          system = "x86_64-linux";
        in
          (import nixpkgs {
            inherit system;
            overlays = [ (self.overlay system) ];
          }).freecad-appimage;
    };
}
