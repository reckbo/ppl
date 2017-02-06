{ pkgs ? import <nixpkgs> {} }:
(pkgs.python27.buildEnv.override {
  extraLibs = builtins.attrValues (import ./pip_packages.nix {
    inherit (pkgs) fetchurl;
    inherit (pkgs.python27Packages) buildPythonPackage;
  });
}).env