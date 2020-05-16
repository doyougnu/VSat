{ nixpkgs ? import ./nixos-20-03.nix }:
let
  pkgs   = import nixpkgs {};
  stdenv = pkgs.stdenv;
in with pkgs; {
  myProject = stdenv.mkDerivation {
    name = "Vsat_Data_Analysis";
    version = "1";
    src = if pkgs.lib.inNixShell then null else nix;

    buildInputs = with rPackages; [
      pandoc
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
