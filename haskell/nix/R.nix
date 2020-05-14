let
  pkgs   = let
    hostPkgs = import <nixpkgs> {};
    pinnedPkgs = hostPkgs.pkgs.lib.importJSON ./nixos-20-03.json;
    _pkgs = hostPkgs.pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (pinnedPkgs) rev sha256;
  };
  in import _pkgs {};
  stdenv = pkgs.stdenv;
in with pkgs; {
  myProject = stdenv.mkDerivation {
    name = "Vsat_Data_Analysis";
    version = "1";
    src = if pkgs.lib.inNixShell then null else nix;

    buildInputs = with rPackages; [
      R
      ggplot2
      knitr
      rmarkdown
      devtools
      reshape2
      yaml
      optparse
      tidyr
      dplyr
      svglite
      cowplot
      latex2exp
      gridExtra
      tidyverse
      forcats
      Hmisc
      ggpubr
      rstatix
    ];
  };
}
