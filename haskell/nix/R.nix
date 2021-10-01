{ nixpkgs ? import ./nixpkgs-01-10.nix }:
let
  pkgs   = import nixpkgs {};
  stdenv = pkgs.stdenv;
in with pkgs; {
  myProject = stdenv.mkDerivation {
    name = "Vsat_Data_Analysis";
    version = "2";
    src = if pkgs.lib.inNixShell then null else nix;

    buildInputs = with rPackages; [
      pandoc
      R
      julia-stable
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
