inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    ({ pkgs, config, lib, secret, ... }:
      let
        inherit (config.magic_rb.pkgs) nixpkgs-unstable;
      in
        with lib;
        {
          magic_rb = {
            pins = inputs;
            overlays = inputs.self.overlays;

            grub = {
              enable = true;
              efi.enable = true;
            };

            hardware.blowhole = true;

            sshdEmacs.enable = true;
            flakes = {
              enable = true;
            };
          };

          services.openssh = {
            enable = true;
            ports = [ 2222 ];
          };
        }
    )
    ({ ... }:
      {
        networking = {
          hostName = "blowhole";
          useDHCP = false;
          interfaces.eno1.useDHCP = false;

          firewall.enable = true;
        };

        time.timeZone = "Europe/Bratislava";
        system.stateVersion = "21.05";

        security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];
      })
  ];
}
