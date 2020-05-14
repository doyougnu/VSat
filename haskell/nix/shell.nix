{ nixpkgs ? import ./nixos-20-03.nix }:
let
  pkgs   = import nixpkgs {};
  inherit (pkgs) haskellPackages;
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install # pkgs.z3
  ];
}
