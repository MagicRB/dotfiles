{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = [
    custom.gpg-key
    nixpkgs.gnupg
  ];

  services.gpg-agent = {
    pinentryFlavor = "gtk2";
    enable = true;
    enableSshSupport = true;
  };

  home.file.".profile".text = ''
     export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';

  home.activation.gnupghome = config.lib.dag.entryAfter ["writeBoundary"] ''
    if [ ! -e ~/.gnupg ]
    then
        ln -sf /mnt/key/gnupg ~/.gnupg
    fi
  '';
}

# another one
