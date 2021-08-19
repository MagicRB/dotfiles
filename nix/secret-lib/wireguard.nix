{ lib, ... }:
with lib;

{
  options = {
    wireguard = mkOption {
      description = ''
        Wireguard machine specific settings.
      '';
      type = with types; attrsOf (submodule
        { options = {
            ips = mkOption {
              description = ''
                IP addresses.
              '';
              type = listOf str;
              default = [];
            };

            listenPort = mkOption {
              description = ''
                Listen port.
              '';
              type = port;
            };

            privateKeyFile = mkOption {
              description = ''
                Path to private key
              '';
              type = str;
            };

            peers = mkOption {
              description = ''
                List of peers.
              '';
              type = listOf (submodule
                {
                  options = {
                    publicKey = mkOption {
                      description = ''
                        Peer public key.
                      '';
                      type = str;
                    };
                    allowedIPs = mkOption {
                      description = ''
                        Allowed IPs for peer.
                      '';
                      type = listOf str;
                    };
                    endpoint = mkOption {
                      description = ''
                        Peer endpoint.
                      '';
                      type = nullOr str;
                      default = null;
                    };
                    persistentKeepalive = mkOption {
                      description = ''
                        Persistent keepalive.
                      '';
                      type = nullOr int;
                      default = null;
                    };
                  };
                }
              );
              default = [];
            };
          };
        }
      );
      default = {};
      example = literalExample
        ''
          {
            heater = {
              ips =
                [ "stuff" ];
              listenPort = 0;
              privateKeyFile = "stuff";
              peers = [ 
                { publicKey =
                    "stuff";
                  allowedIPs =
                    [ "stuff"
                    ];
                };
              ];
            }
          }
        '';
    };
  };
}
