{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib, inputs }:
{ config, lib, ... }:
{
  home.file.".config/pulse/daemon.conf".source = ./daemon.conf;
}
