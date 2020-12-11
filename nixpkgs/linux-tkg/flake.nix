{
  inputs.nixpkgs.url = "nixpkgs";
  inputs.linux-tkg = {
    flake = false;
    url = "git+https://github.com/Frogging-Family/linux-tkg";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: inputs.nixpkgs.lib.genAttrs supportedSystems (system: f system);
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in
      rec {
        overlay = let
          linux-tkg = { fetchurl, buildLinux, ... } @ args:
            buildLinux (args // rec {
              version = "5.4.0-rc3";
              modDirVersion = version;

              src = fetchurl {
                url = "https://github.com/jsakkine-intel/linux-sgx/archive/v23.tar.gz";
                sha256 = "11rwlwv7s071ia889dk1dgrxprxiwgi7djhg47vi56dj81jgib20";
              };
              kernelPatches = [];

              extraConfig = ''
            INTEL_SGX y
          '';

              extraMeta.branch = "5.4";
            } // (args.argsOverride or {}));
        in 
          linux-tkg;
        defaultPackage = forAllSystems (system: (import inputs.nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).emacs);
      };
}
