{ pkgs, ... }: with pkgs.lib;
  {
    instances = mkOption {
      description = "Klippy instances";
      default = {};
      example = {
        package = pkgs.klippy;
        inputTty = "/tmp/printer";
        logFile = "/var/log/klippy.log";
      };
      type = types.attrsOf (types.submodule {
        options = {
          package = mkOption {
            default = pkgs.klippy;
            type = types.package;
            description = ''
              klippy package
            '';
          };

          inputTty = mkOption {
            default = "/tmp/printer";
            type = types.str;
            description = ''
              inputs tty name 
            '';
          };

          apiServer = mkOption {
            default = "/tmp/klippy_uds";
            type = types.str;
            description = ''
              api server unix domain socket filename 
            '';
          };

          logFile = mkOption {
            default = "";
            type = types.str;
            description = ''
              write log to file instead of stderr
            '';
          };

          verbose = mkOption {
            default = false;
            type = types.bool;
            description = ''
              enable debug messages 
            '';
          };

          dictionary = mkOption {
            default = "";
            type = types.str;
            description = ''
              file to read for mcu protocol dictionary
            '';
          };

          config = mkOption {
            default = {};
            type = types.submodule {
              options = {
                file = mkOption {
                  default = "";
                  type = types.path;
                  description = ''
                  klipper config file
                '';
                };

                extraImports = mkOption {
                  default = [];
                  type = types.listOf types.path;
                  description = ''
                  extra imports added at the end of the config
                '';
                };

                virtualSd = mkOption {
                  default = "";
                  type = types.path;
                  description = ''
                  virtual sd folder
                '';
                };
              };
            };
          };
        };
      });
    };

    user = mkOption {
      default = "klippy";
      type = types.str;
      description = ''
          the user for klippy   
        '';
    };

    uid = mkOption {
      default = 5687;
      type = types.int;
      description = ''
          the uid for the user for klippy
        '';
    };

    group = mkOption {
      default = "klippy";
      type = types.str;
      description = ''
          the group for klippy
        '';
    };

    gid = mkOption {
      default = 5687;
      type = types.int;
      description = ''
          the gid for the group for klippy
        '';
    };

    extraGroups = mkOption {
      default = [ "dialout" ];
      type = types.listOf types.str;
      description = ''
          extra groups for klippy
        '';
    };
  }
