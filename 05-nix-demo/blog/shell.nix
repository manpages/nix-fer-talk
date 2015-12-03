{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  f = import ./project.nix;
  drv = pkgs.haskell.packages.${compiler}.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv

