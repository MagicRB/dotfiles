inputs: {
  system = "x86_64-linux";

  # hm."main" = import ../home-manager/profiles/mark.nix;

  modules = [
    ../hardware/default.nix # manual
    ../nixos-modules/pin-nixpkgs.nix # manual
    ../nixos-modules/main.nix # auto
  ] ++ [
    ({ pkgs, config, ... }: {
      magic_rb = {
        pins = inputs;
        config = {
          allowUnfree = true;
        };
        overlays = inputs.self.overlays;

        hardware.mark = true;
      };
      time.timeZone = "Europe/Bratislava";
      system.stateVersion = "20.09";

      environment.systemPackages = with pkgs; [
        gnupg
        pinentry
        openssl
        paperkey
        monkeysphere
        magic_rb.sss-cli
      ];
    })
  ];
}
