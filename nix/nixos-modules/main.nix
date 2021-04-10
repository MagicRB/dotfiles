{ config, lib, ... }:
with lib;
let
  nm-enable = config.networking.networkmanager.enable;
  docker-enable = config.virtualisation.docker.enable;
in {
  users = {
    mutableUsers = false;
    
    users.main = {
      isNormalUser = true;
      home = "/home/main";
      hashedPassword = "<REDACTED>";
      description = "main";

      uid = 1000;

      extraGroups = [ "wheel" "audio" ]
                     ++ (optional nm-enable "network-manager")
                    ++ (optional docker-enable "docker");
    };
    
    groups.main = {
      gid = 1000;
    };
  };
}
