inputs: {
  system = "aarch64-linux";
  username = "u0_a269";
  homeDirectory = "/data/data/com.termux/files/home";

  configuration =
    { pkgs, ... }: {
      magic_rb = {
        pins = inputs;
        overlays = inputs.self.overlays;

        programs = {
          bash.enable = true;
          emacs = {
            enable = true;
            package = pkgs.emacs;
          };
        };
      };

      imports = [ ../home-manager/modules/default.nix ];

      home.stateVersion = "20.09";
    };
}
