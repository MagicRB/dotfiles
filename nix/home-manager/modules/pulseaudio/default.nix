{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, lib, ... }:
{
  home.file.".config/pulse/daemon.conf".source = ./daemon.conf;
}
