{ nixpkgs, nixpkgs-unstable, nixpkgs-master, custom, hostname, rlib }:
{ config, ... }:
{
  hardware.nvidia.package = config.hardware.nvidia.package.overrideAttrs (old:
    {
      patches = old ++ [ ./nvidia-5.11.patch ];
    });
}
