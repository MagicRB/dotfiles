inputs: {
  system = "aarch64-linux";

  modules = [
    ../nixos-modules/default.nix

    ({ pkgs, config, lib, ... }:
      {
        magic_rb = {
          grub = {
            enable = true;
            efi.enable = true;
          };

          pins = inputs;
          overlays = inputs.self.overlays;

          hardware.gooseberry = true;
          sshdEmacs.enable = true;
          flakes.enable = true;
          vpsRemoteAccess =
            { enable = true;
              trustedWheel = true;
            };
        };
          
        services.openssh = {
          enable = true;
        };
          
        networking = {
          hostName = "gooseberry";
          useDHCP = false;
          interfaces.eth0.useDHCP = false;

          firewall.enable = true;
          wireless =
            { enable = true;
              interfaces = [ "wlan0" ];
            };
        };

        time.timeZone = "Europe/Bratislava";
        system.stateVersion = "21.05";

        security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];
      })
  ];
}
