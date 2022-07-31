# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  inputs,
  lib,
  withSystem,
  self,
  roots,
  ...
}:
with lib; let
  mkNixos = {
    system,
    name,
  }: module: let
    module' =
      if isFunction module
      then module
      else if isPath module || isString module
      then import module
      else throw "Expected `function`, `path` or `string`, but got `${typeOf module}`";
  in
    withSystem system
    ({pkgs, ...}:
      nixosSystem
      {
        inherit system;
        specialArgs = {
          inherit self inputs roots;
        };

        modules = [
          module'
          ({config, ...}: {
            networking.hostName = name;
            nixpkgs.overlays = mapAttrsToList (const id) self.overlays;
            nixpkgs.pkgs = import inputs.nixpkgs {
              inherit system;
              inherit (config.nixpkgs) overlays config;
            };
          })
        ];
      });

  eachNixosSystem = path: let
    all = builtins.readDir path;
    filtered = filterAttrs (n: v: hasSuffix ".nix" n || v == "directory" ) all;
    systemFiles = mapAttrsToList (n: _: n) filtered;
    systems = map (file: import "${path}/${file}") systemFiles;
    systemConfigurations =
      map
      (
        system: {
          ${system.name} = mkNixos {inherit (system) name system;} system.module;
        }
      )
      systems;
  in
    foldr mergeAttrs {} systemConfigurations;
in {
  flake.nixosConfigurations = eachNixosSystem "${roots.nixos}/systems";

  perSystem = {pkgs, system, ...}: {
    apps = inputs.nixinate.nixinate.${system} self;
  };
}
