inputs: {
  system = "x86_64-linux";
  username = "main";
  homeDirectory = "/home/main";

  configuration =
    { pkgs, ... }: {
      home.stateVersion = "20.09";

      magic_rb = {
        pins = {
          inherit (inputs)
            nixpkgs
            nixpkgs-unstable
            nixpkgs-master

            home-manager
            nixng
            fenix;
        };
        overlays = inputs.self.overlays;

        programs = {
          bash = {
            enable = true;
            enableDirenv = true;
          };
          ssh.enable = true;
        };
      };

      imports = [ ../home-manager/modules/default.nix ];
    };
}
