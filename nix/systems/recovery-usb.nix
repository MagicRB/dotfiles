inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    (_: {
      magic_rb = {
        pins = {
          inherit (inputs)
            nixpkgs
            nixpkgs-unstable
            nixpkgs-master

            home-manager
            nixng;
        };
        config = {
          allowUnfree = true;
        };
        overlays = inputs.self.overlays;
        hardware.recoveryUsb = true;
      };
    })
    (import "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
  ];
}
