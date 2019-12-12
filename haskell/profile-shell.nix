let nixpkgs = import <nixpkgs> {};
    orig = nixpkgs.pkgs.profiledHaskellPackages.callPackage ./default.nix {};
in (nixpkgs.pkgs.haskell.lib.doBenchmark orig).env
