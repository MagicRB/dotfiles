{ hostName, hostId }:
inputs: {
  system = "x86_64-linux";

  modules = [
    ../nixos-modules/default.nix
    ({ pkgs, config, ... }:
      {
        magic_rb = {
          grub = {
            enable = true;
            efi.enable = false;
            devices = [ "/dev/disk/by-id/scsi-360646ec4d8e14b45b588dcafaf0b511b" ];
          };

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

          hardware."${hostName}" = true;
          flakes.enable = true;
        };

        services.sshd = {
          enable = true;
          passwordAuthentication = true;
          permitRootLogin = "no";
        };

        networking = {
          firewall = {
            allowedTCPPorts = [ 22 ];
          };

          useDHCP = true;
          # interfaces.enp3s0.useDHCP = true;

          firewall.enable = true;
          inherit hostId hostName;
        };

        time.timeZone = "Europe/Bratislava";
        system.stateVersion = "20.09";
        security.pki.certificates = [ (builtins.readFile ../redalder.org.crt) ];
      })
  ];

}
