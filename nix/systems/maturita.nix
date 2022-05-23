inputs: {
  system = "x86_64-linux";
  username = "riso";
  homeDirectory = "/home/riso";

  configuration =
    { pkgs, ... }: {
      magic_rb = {
        pins = inputs;
        overlays = inputs.self.overlays;

        programs = {
          bash.enable = true;
          emacs = {
            enable = true;
            enableMu4e = false;
          };
        };
      };

      imports = [ ../home-manager/modules/default.nix ];

      home.stateVersion = "20.09";
    };
}
