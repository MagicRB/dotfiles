{
  inputs = {
    nixpkgs.url = "nixpkgs";

    klippy = {
      url = "github:KevinOConnor/klipper?rev=e68cf08d15a985ecce7497b58408ee233dd54eb9";
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
            klippy = let
              pythonPackages = python-packages: with python-packages; [
                greenlet
                cffi
                pyserial
                jinja2
              ]; 
              pythonWithPackages = (python2.withPackages pythonPackages);
            in stdenv.mkDerivation
              {
                name = "klippy";
                src = inputs.klippy;
                buildInputs = with pkgs; [ makeWrapper ];

                dontBuild = true;
                installPhase = ''
                  mkdir -p $out/share/klippy
                  cp -r klippy/* $out/share/klippy

                  mkdir -p $out/bin
                  chmod +x $out/share/klippy/klippy.py
                  makeWrapper \
                    $out/share/klippy/klippy.py \
                    $out/bin/klippy \
                    --prefix PATH : ${lib.makeBinPath (with pkgs; [
                      pythonWithPackages
                    ])}

                  ${pythonWithPackages}/bin/python $out/share/klippy/chelper/__init__.py
                '';              
              };
            
          };

        defaultPackage = forAllSystems (system: (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).klippy);

        nixosModules.klippy = { pkgs, config, ... }: with nixpkgs.lib;
          let
            cfg = config.services.klippy;
          in {
            options.services.klippy = import ./options.nix { inherit pkgs config; };

            config =
              {
                nixpkgs.overlays = [ self.overlay ];

                users.users."${cfg.user}" = {
                  uid = cfg.uid;
                  description = "klippy user";
                  group = "${cfg.group}";
                  extraGroups = cfg.extraGroups;
                };

                users.groups."${cfg.group}" = {
                  gid = cfg.gid;
                };
                
                systemd.services = mapAttrs'
                  (name: value:
                    let
                      configFile = pkgs.writeText "printer.cfg" ''
                        ${builtins.readFile value.config.file}

                        ${foldl (str: acc: acc + str) "" (map (x: "[include " + x + "]\n") value.config.extraImports)}
                        
                        ${if value.config.virtualSd != "" then "[virtual_sdcard]\npath: " + value.config.virtualSd else ""}
                      '';
                    in
                    (nameValuePair
                      ("klippy-" + name)
                      {
                        wantedBy = [ "multi-user.target" ];
                        after = [ "network.target" ];
                        description = "Starts klipper-" + name;
                        serviceConfig = {
                          Type = "simple";
                          User = cfg.user;
                          Group = cfg.group;
                          RemainAfterExit = "yes";
                          ExecStart = ''
                            ${value.package}/bin/klippy \
                              --input-tty ${value.inputTty} \
                              ${if value.apiServer != "" then "--api-server " + value.apiServer else ""} \
                              ${if value.logFile != "" then "--logfile " + value.logFile else ""} \
                              ${if value.verbose then "-v" else ""} \
                              ${if value.dictionary != "" then "--dictionary " + value.dictionary else ""} \
                              ${configFile}
                          '';
                          ExecStartPost = "${pkgs.bash}/bin/bash -c '(sleep 5 ; [[ -e \"${value.apiServer}\" ]] && chmod 664 \"${value.apiServer}\" ) & disown'";
                          Restart = "always";
                          RestartSec = 10;
                        };
                      }))
                  cfg.instances;
              };
          };
      };
}
