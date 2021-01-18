{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-20.09";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master"; 

    sss-cli = {
      flake = false;
      url = "github:dsprenkels/sss-cli";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      klippyModule = (import ./packages/klippy).nixosModules.klippy;
      moonrakerModule = (import ./packages/moonraker).nixosModules.moonraker;
      mainsailModule = (import ./packages/mainsail).nixosModules.mainsail;
    in {
      nixosConfigurations.omen = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./filesystem/omen.nix
          ./kernel/omen.nix

          ({ pkgs, ... }: {
            

            networking = {
              hostName = "omen";
              useDHCP = false;
              interfaces.eno1.useDHCP = true;
            };

            time.timeZone = "Europe/Bratislava";
            system.stateVersion = "20.09";
          })

          (import ./profiles/laptop.nix {
            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          })

          (import ./modules/pin-nixpkgs.nix inputs)

          ./users/main.nix
        ];
      };

      nixosConfigurations.heater = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./filesystem/heater.nix
          (import ./kernel/heater.nix (import inputs.nixpkgs-unstable { system = "x86_64-linux"; config.allowUnfree = true; }))

          ({ pkgs, ... }: {
            networking = {
              hostName = "heater";
              useDHCP = false;
              interfaces.enp3s0.useDHCP = true;
            };

            time.timeZone = "Europe/Bratislava";
            system.stateVersion = "20.09";

            virtualisation.docker.enable = true;
          })

          (import ./profiles/workstation.nix)

          (import ./modules/pin-nixpkgs.nix inputs)

          ./users/main.nix
        ];
      };
    };
}
