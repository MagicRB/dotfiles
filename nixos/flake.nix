{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-20.09";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master"; 
  };

  outputs = { self, nixpkgs, ... }@inputs: let
  in {
    nixosConfigurations.omen = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";

      modules = [
        ./filesystem/omen.nix
        ./kernel/omen.nix

        (_: {
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
        ./kernel/heater.nix

        (_: {
          networking = {
            hostName = "heater";
            useDHCP = false;
            interfaces.enp3s0.useDHCP = true;
          };

          time.timeZone = "Europe/Bratislava";
          system.stateVersion = "20.09";
        })

        (import ./profiles/workstation.nix)

        (import ./modules/pin-nixpkgs.nix inputs)

        ./users/main.nix
      ];
    };
  };
}
