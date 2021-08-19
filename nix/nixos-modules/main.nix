{ config, lib, secret, ... }:
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
      hashedPassword = secret.passwordHashes.main.generic;
      description = "main";

      uid = 1000;

      extraGroups = [ "wheel" "audio" ]
                     ++ (optional nm-enable "network-manager")
                    ++ (optional docker-enable "docker");

      openssh.authorizedKeys.keys =
        [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFVkFvalffJ/SMjJGG3WPiqCqFygnWzhGUaeALBIoCsJ (none)" ];
    };
    
    groups.main = {
      gid = 1000;
    };
  };
}
