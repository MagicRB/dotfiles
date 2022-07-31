# SPDX-FileCopyrightText: 2022 Richard Brežák <richard@brezak.sk>
#
# SPDX-License-Identifier: LGPL-3.0-or-later
{
  lib,
  inputs,
  config,
  perSystem,
  roots,
  ...
}:
with lib; let
  cfg = config.flake.overlays;

  packageSets = let
    all = builtins.readDir overlayDir;
    overlayFiles = filterAttrs (n: v: n != "default.nix" && (hasSuffix ".nix" n || v == "directory")) all;
    overlays =
      mapAttrs'
      (
        file: _: let
          packageSet = import "${overlayDir}/${file}";
          overlay = callPackageWith inputs packageSet.overlay {};

          dependencies =
            map
            (n: overlays.${n}.overlay)
            (packageSet.overlays or []);
          foldedOverlay =
            foldOverlays
            (dependencies ++ singleton overlay);
        in
          nameValuePair
          packageSet.name
          {
            overlay = foldedOverlay;
            systems = packageSet.systems or config.systems;
            rawOverlay = overlay;
            packages = mapAttrsToList (n: _: n) (overlay null null);
          }
      )
      overlayFiles;
  in
    overlays;

  foldOverlays = overlays: final: prev:
    foldl
    (acc: overlay: acc // overlay final acc)
    prev
    overlays;

  traceOverlay = overlay:
    trace (builtins.attrNames (overlay {} {})) overlay;

  overlayDir = "${roots.flake}/overlays";
in {
  flake.overlays = mapAttrs (_: v: v.rawOverlay) packageSets;

  perSystem = {
    config,
    system,
    pkgs,
    ...
  }: {
    packages =
      foldl
      (acc: attrs: recursiveUpdate acc attrs)
      {}
      (flatten
        (flip mapAttrsToList
          (filterAttrs (_: v: elem system v.systems) packageSets)
          # packageSets
          (_: packageSet: let
            pkgs = import inputs.nixpkgs {
              overlays = singleton packageSet.overlay;
              inherit system;
            };
          in (filterAttrs (const isDerivation) (genAttrs packageSet.packages (n: pkgs.${n}))))));
  };
}
