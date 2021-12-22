inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    inputs.home-manager.nixosModules.home-manager
    (_: {
      home-manager.users."main" =
        { ... }: {
          imports = [ ../home-manager/modules/default.nix ];

          magic_rb = {
            pins = inputs;
            config = {
              allowUnfree = true;
            };
            overlays = inputs.self.overlays;

            programs = {
              bash.enable = true;
              ssh.enable = true;
            };
          };

          home.stateVersion = "20.09";
        };
      magic_rb = {
        pins = inputs;
        config = {
          allowUnfree = true;
        };
        overlays = inputs.self.overlays;
        hardware.recoveryUsb = true;
        flakes.enable = true;
      };
    })
    (import "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
  ];
}
