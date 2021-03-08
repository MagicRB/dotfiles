{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.packages = [
    custom.gpg-key
    nixpkgs.gnupg
  ];

  home.file.".gpg-agent.conf".text = (pinentryFlavor: ''
    enable-ssh-support
    pinentry-program ${nixpkgs.pinentry.${pinentryFlavor}}/bin/pinentry
  '') "gtk2";

  home.file.".profile".text = ''
     export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';

  home.activation.gnupghome = config.lib.dag.entryAfter ["writeBoundary"] ''
    if [ ! -e ~/.gnupg ]
    then
        ln -sf /mnt/key/gnupg ~/.gnupg  
    fi

    if [ ! -e ~/.gnupg/gpg-agent.conf ]
    then
        ln -sf ~/.gpg-agent.conf /mnt/key/gnupg/gpg-agent.conf
    fi
  '';
}
