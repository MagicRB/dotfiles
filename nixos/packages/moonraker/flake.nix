{
  inputs = {
    nixpkgs.url = "nixpkgs";

    moonraker = {
      url = "github:MagicRB/moonraker?ref=master";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
      {
        overlay = final: prev:
          with final; {
            moonraker = let
              pythonPackages = python-packages: with python-packages; [
                tornado
                pyserial
                pillow
              ];
              pythonWithPackages = (python3.withPackages pythonPackages);
            in stdenv.mkDerivation
              {
                name = "moonraker";
                src = inputs.moonraker;
                buildInputs = with pkgs; [ makeWrapper ];

                dontBuild = true;
                installPhase = ''
                  mkdir -p $out/share/moonraker
                  cp -r * $out/share/moonraker

                  mkdir -p out/bin
                  chmod +x $out/share/moonraker/moonraker/moonraker.py

                  makeWrapper \
                    $out/share/moonraker/moonraker/moonraker.py \
                    $out/bin/moonraker \
                    --prefix PATH : ${lib.makeBinPath (with pkgs; [
                      pythonWithPackages
                    ])}
                '';
              };
          };

        nixosModules.moonraker = { pkgs, config, ... }: with nixpkgs.lib;
          let
            cfg = config.services.moonraker;
          in {
            options.services.moonraker = import ./options.nix { inherit pkgs config; };

            config =
              {
                nixpkgs.overlays = [ self.overlay ];

                users.users."${cfg.user}" = mkIf cfg.createUser {
                  uid = cfg.uid;
                  description = "klippy user";
                  group = "${cfg.group}";
                  extraGroups = cfg.extraGroups;
                };

                users.groups."${cfg.group}" = mkIf cfg.createGroup {
                  gid = cfg.gid;
                };

                systemd.services = mapAttrs'
                  (name: value:
                    let
                      configFile = pkgs.writeText "moonraker.conf" (import ./config.nix value pkgs.lib);
                    in (nameValuePair
                      ("moonraker-" + name)
                      {
                        wantedBy = [ "muti-user.target" ];
                        after = [ "network.target" ];
                        description = "Starts moonraker-" + name;
                        serviceConfig = {
                          Type = "simple";
                          User = cfg.user;
                          Group = cfg.group;
                          RemainAfterExit = "yes";
                          ExecStart = ''
                            ${value.package}/bin/moonraker \
                              --configfile ${configFile} \
                              --logfile ${value.logFile}
                          '';
                          Restart = "always";
                          RestartSec = 10;
                        };
                      }))
                  cfg.instances;
              };
          };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).moonraker);
      };
}

  
