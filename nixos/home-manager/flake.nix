{
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager?ref=release-20.09";
    };

    nixpkgs.url = "nixpkgs";
    nixpkgs-unstable.url = "nixpkgs-unstable";
    nixpkgs-master.url = "nixpkgs-master";
    
    sss-cli = {
      url = "github:dsprenkels/sss-cli";
      flake = false;
    };
    
    emacs-overlay.url = "git+https://github.com/nix-community/emacs-overlay";
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
                    emacs = lib.halfCallFlake ./packages/emacs;
                    screenshot = lib.halfCallFlake ./packages/screenshot;
                    emacsclient-remote = lib.halfCallFlake ./packages/emacsclient-remote;
                    #atom-shell = lib.halfCallFlake ./packages/atom-shell;
                    sss-cli = lib.halfCallFlake ./packages/sss-cli;
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

        heater = inputs.home-manager.lib.homeManagerConfiguration {
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
                    emacs = lib.halfCallFlake ./packages/emacs;
                    screenshot = lib.halfCallFlake ./packages/screenshot;
                    emacsclient-remote = lib.halfCallFlake ./packages/emacsclient-remote;
                    #atom-shell = lib.halfCallFlake ./packages/atom-shell;
                    sss-cli = lib.halfCallFlake ./packages/sss-cli;
                  };
                };
              dotfiles = ~/dotfiles;
            in {
              home.packages = [
                (import ./packages/enter-env pkgs)
                pkgs.nixpkgs-unstable.nomad_1_0
                pkgs.custom.sss-cli
              ];

              home.stateVersion = "20.09";

              imports = [
                (import ./modules/alacritty pkgs)
                (import ./modules/bash pkgs)
                (import ./modules/cmdline-utils.nix pkgs)
                (import ./modules/dunst pkgs)
                (import ./modules/emacs pkgs)
                (import ./modules/graphical-programs.nix pkgs)

                (import ./modules/webdev.nix pkgs)

                (import ./modules/i3 pkgs "heater")
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
    heater = self.homeConfigurations.heater.activationPackage;
  };
}
