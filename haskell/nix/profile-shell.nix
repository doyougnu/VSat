let nixpkgs = import ./nixos-20-03.nix {};
    orig = nixpkgs.pkgs.profiledHaskellPackages.callPackage ./default.nix {};
in (nixpkgs.pkgs.haskell.lib.doBenchmark orig).env
