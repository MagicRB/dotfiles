{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.packages = [
    custom.gpg-key
  ];

  home.activation.gnupghome = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf /mnt/key/gnupg ~/.gnupg
  '';
}
