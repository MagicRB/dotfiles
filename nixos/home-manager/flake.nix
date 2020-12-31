{
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager?ref=release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "nixpkgs";
    nixpkgs-unstable.url = "nixpkgs-unstable";
    nixpkgs-master.url = "nixpkgs-master";
  };

  outputs = { self, ... }@inputs: {
    homeConfigurations = 
      let
        lib = import ./lib.nix
          inputs
          { lib = inputs.nixpkgs.lib; system = "x86_64-linux"; };
      in {
        omen = inputs.home-manager.lib.homeManagerConfiguration {
          configuration = { pkgs, ... }:
            let
              pkgs =
                let
                  pkgs = lib.getLegacyPkgs
                    {
                      allowUnfree = true;
                    }
                    {
                      nixpkgs = inputs.nixpkgs;
                      nixpkgs-unstable = inputs.nixpkgs-master;
                      nixpkgs-master = inputs.nixpkgs-unstable;
                    };
                in pkgs // {
                  custom = {
                    emacs = (import ./packages/emacs).defaultPackage."x86_64-linux";
                    screenshot = (import ./packages/screenshot).defaultPackage."x86_64-linux";
                    emacsclient-remote = (import ./packages/emacsclient-remote).defaultPackage."x86_64-linux";
                  };
                };
              dotfiles = ~/dotfiles;
            in {
              home.packages = [ (import ./packages/enter-env pkgs) ];

              home.stateVersion = "20.09";

              imports = [
                (import ./modules/alacritty pkgs)
                (import ./modules/bash pkgs)
                (import ./modules/cmdline-utils.nix pkgs)
                (import ./modules/dunst pkgs)
                (import ./modules/emacs pkgs)
                (import ./modules/graphical-programs.nix pkgs)

                (import ./modules/i3 pkgs "omen")
                (import ./modules/nix-du.nix pkgs)
                (import ./modules/picom pkgs)

                (import ./modules/urxvt.nix pkgs)
              ];
            };

          system = "x86_64-linux";
          homeDirectory = "/home/main";
          username = "main";
        };
      };

    omen = self.homeConfigurations.omen.activationPackage;
    heater = self.homeConfigurations.omen.activationPackage;
  };
}
