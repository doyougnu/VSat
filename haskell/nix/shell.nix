{ nixpkgs ? import ./nixos-20-03.nix, compiler ? "ghc833" }:
let
  pkgs   = import nixpkgs {};
  inherit (pkgs) haskellPackages;
  ghc = pkgs.haskell.packages.${compiler}.callPackage ./default.nix { };
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install # pkgs.z3
  ];
}
