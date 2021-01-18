{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-20.09";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs?ref=master";
    
    ra-systems = {
      url = "git+https://gitea.redalder.org/RedAlder/systems";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager?ref=release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    #  PACKAGES
    ## sss-cli
    sss-cli = {
      flake = false;
      url = "github:dsprenkels/sss-cli";
    };

    ## Emacs
    emacs-overlay = {
      url = "git+https://github.com/nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      type = "git";
      url = "https://git.savannah.gnu.org/git/emacs.git";
      ref = "feature/native-comp";
      flake = false;
    };
    vtermModule = {
      url = "git+https://github.com/akermu/emacs-libvterm";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    with inputs; let
      customModules = [
        ra-systems.flakes.klippy.nixosModules.klippy
        ra-systems.flakes.moonraker.nixosModules.moonraker
        ra-systems.flakes.mainsail.nixosModules.mainsail
        inputs.home-manager.nixosModules.home-manager
      ];

      rlib = (import ./lib.nix inputs { lib = nixpkgs.lib; system = "x86_64-linux"; });
      rpkgs = (rlib.getLegacyPkgs
        {
          allowUnfree = true;
        }
        {
          inherit (inputs) nixpkgs nixpkgs-unstable nixpkgs-master;
        }) // {
          custom = {
            sss-cli = rlib.halfCallFlakePackage ./packages/sss-cli;
            atom-shell = rlib.halfCallFlakePackage ./packages/atom-shell;
            emacs = rlib.halfCallFlakePackage ./packages/emacs;
            emacsclient-remote = rlib.halfCallFlakePackage ./packages/emacsclient-remote;
            enter-env = rlib.halfCallFlakePackage ./packages/enter-env;
            screenshot = rlib.halfCallFlakePackage ./packages/screenshot;
          };
          inherit rlib;
        };
    in {
      nixosConfigurations.omen = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = rlib.callModules rpkgs [
          ./hardware/omen.nix
          ./modules/pin-nixpkgs.nix
          ./users/main.nix
        ] ++ [
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
          } inputs rpkgs)
        ] ++ customModules;
      };

      nixosConfigurations.heater = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = rlib.callModules rpkgs [
          ./hardware/heater.nix
          ./profiles/workstation.nix
          ./modules/pin-nixpkgs.nix
          ./users/main.nix
        ] ++ [
          ({ pkgs, ... }: {
            networking = {
              hostName = "heater";
              useDHCP = false;
              interfaces.enp3s0.useDHCP = true;
            };

            time.timeZone = "Europe/Bratislava";
            system.stateVersion = "20.09";

            virtualisation.docker.enable = true;

            home-manager.users.main = rlib.callModule rpkgs ./hm-profiles/common.nix;
          })
        ] ++ customModules;
      };

      nixosConfigurations.mark = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = rlib.callModules rpkgs [
          ./hardware/mark.nix
        ] ++ [
          ({ pkgs, ... }: {
            time.timeZone = "Europe/Bratislava";
            environment.systemPackages = [
              pkgs.gnupg
              (let
                lib = import ./lib.nix
                  inputs
                  { lib = inputs.nixpkgs.lib; system = "x86_64-linux"; };
              in
                lib.halfCallFlake ./packages/sss-cli)
            ];
              system.stateVersion = "20.09";
          })
        ];
      };
    };
}
