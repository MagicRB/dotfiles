{ config, pkgs, ... }:
let
  nm-enable = config.networking.networkmanager.enable;
  docker-enable = config.virtualisation.docker.enable;
  mkIf = pkgs.lib.mkIf;
in {
  users = {
    mutableUsers = false;
    
    users.main = {
      isNormalUser = true;
      home = "/home/main";
      hashedPassword = "<redacted>";
      description = "main";

      uid = 1000;

      extraGroups = [ "wheel" "audio" ]
                    ++ (if nm-enable then [ "networkmanager" ] else [])
                    ++ (if docker-enable then [ "docker" ] else []);
    };
    
    groups.main = {
      gid = 1000;
    };
  };
}
