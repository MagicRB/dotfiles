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

  home.activation.gnupghome = config.lib.dag.entryAfter ["writeBoundary"] ''
    if [ ! -e ~/.gnupg ]
    then
        ln -sf /mnt/key/gnupg ~/.gnupg
    fi
  '';
}
