let
  pkgs = import <unstable> { };
  agda = pkgs.agda.withPackages (apkgs: with apkgs;
    [ standard-library
    ]
  );
in
with pkgs;

mkShell
{ buildInputs = [ agda ];
  shellHook = ''
  export AGDA_DIR="~/.agda";
'';
}
