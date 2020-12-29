{ pkgs, ... }: with pkgs.lib; {
  instances = mkOption {
    default = {};
    description = "Moonraker instances";
    type = types.attrsOf (types.submodule {
      options = {
        package = mkOption {
          default = pkgs.moonraker;
          type = types.package;
          description = ''
            moonraker package
          '';
        };

        config = mkOption {
          default = {};
          type = types.submodule {
            options = {
              server = mkOption {
                default = {};
                type = types.submodule {
                  options = {
                    host = mkOption {
                      default = "0.0.0.0";
                      type = types.str;
                      description = ''
                        The host address in which to bind the HTTP server.
                      '';
                    };

                    port = mkOption {
                      default = 7125;
                      type = types.int;
                      description = ''
                        The port the HTTP server will listen on.
                      '';
                    };

                    klippyUdsAddress = mkOption {
                      default = "/tmp/klippy_uds";
                      type = types.str;
                      description = ''
                        The address of Unix Domain Socket used to communicate with Klippy.
                      '';
                    };

                    maxUploadSize = mkOption {
                      default = 200;
                      type = types.int;
                      description = ''
                        The maximum size allowed for a file upload.  Default is 200 MiB.
                      '';
                    };

                    enableDebugLogging = mkOption {
                      default = true;
                      type = types.bool;
                      description = ''
                        When set to True Moonraker will log in verbose mode.
                      '';
                    };

                    configPath = mkOption {
                      default = "";
                      type = types.str;
                      description = ''
                        The path to a directory where configuration files are located.
                      '';
                    };
                  };
                };
              };

              authorization = mkOption {
                default = {};
                type = types.submodule {
                  options = {
                    enabled = mkOption {
                      default = true;
                      type = types.bool;
                      description = ''
                        Enables authorization.
                      '';
                    };
                    
                    apiKeyFile = mkOption {
                      default = "/tmp/.moonraker_api_key";
                      type = types.str;
                      description = ''
                        Path of the file that stores Moonraker's API key.
                      ''; 
                    };

                    trustedClients = mkOption {
                      default = [];
                      type = types.listOf types.str;
                      description = ''
                        A list of newline separated ip addresses and/or ip ranges that are trusted.
                      '';
                    };

                    corsDomains = mkOption {
                      default = [];
                      type = types.listOf types.str;
                      description = ''
                        Enables CORS for the specified domains.
                      '';
                    };
                  };
                };
              };
            };
          };
        };

        logFile = mkOption {
          default = "/tmp/moonraker.log";
          type = types.str;
          description = ''
            moonraker log file
          '';
        };
      };
    });
  };

  
  user = mkOption {
    default = "moonraker";
    type = types.str;
    description = ''
      moonraker user
    '';
  };

  uid = mkOption {
    default = 5688;
    type = types.int;
    description = ''
      moonraker user id
    '';
  };

  group = mkOption {
    default = "moonraker";
    type = types.str;
    description = ''
      moonraker group
    '';
  };

  gid = mkOption {
    default = 5688;
    type = types.int;
    description = ''
      moonraker group id
    '';
  };

  extraGroups = mkOption {
    default = [ "klippy" ];
    type = types.listOf types.str;
    description = ''
      extra groups for moonraker user
    '';
  };

  createUser = mkOption {
    default = true;
    type = types.bool;
    description = ''
      create user
    '';
  };

  createGroup = mkOption {
    default = true;
    type = types.bool;
    description = ''
      create group
    '';
  };
}
