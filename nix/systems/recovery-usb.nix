inputs: {
  system = "x86_64-linux";

  # hm."main" = import ../home-manager/profiles/mark.nix;

  modules = [
    (_: {
      supportedFilesystems = [ "zfs" ];
      magic_rb = {
        pins = {
          "nixpkgs" = inputs.nixpkgs;
          "nixpkgs-unstable" = inputs.nixpkgs-unstable;
          "nixpkgs-master" = inputs.nixpkgs-master;

          "nixng" = inputs.nixng;
        };
        config = {
          allowUnfree = true;
        };
        overlays = inputs.self.overlays;
      };
    })
    (import "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
  ];

  compatModules = [
    
  ];
}
