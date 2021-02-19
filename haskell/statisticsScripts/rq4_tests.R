library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

finRawFile  <- "../munged_data/financial_raw_singletons.csv"
autoRawFile <- "../munged_data/auto_raw_singletons.csv"

finRawDF <- read.csv(file=finRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Financial") %>%
  group_by(Algorithm, DataSet, Config) %>%
  mutate(TimeCalc = Time - lag(Time, default = 0))

autoRawDF <- read.csv(file=autoRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Auto") %>%
  group_by(Algorithm, DataSet, Config) %>%
  mutate(TimeCalc = Time - lag(Time, default = 0))

##################### Financial #############################
## Algorithms are significant
fin.alg.res <- kruskal.test(TimeCalc ~ Algorithm, finRawDF)

## Versions are significant as expected
fin.vers.res <- kruskal.test(TimeCalc ~ Config, finRawDF)

## Solvers also significant
fin.slvr.res <- kruskal.test(TimeCalc ~ DataSet, finRawDF)

## Interaction bewtween algorithm and version significant as expected
fin.alg.conf.inters      <- interaction(finRawDF$Algorithm, finRawDF$Config)
fin.alg.slvr.inters      <- interaction(finRawDF$Algorithm, finRawDF$DataSet)
fin.alg.conf.slvr.inters <- interaction(finRawDF$DataSet, finRawDF$Algorithm, finRawDF$Config)

fin.alg.conf.res      <- kruskal.test(TimeCalc ~ fin.alg.conf.inters, finRawDF)
fin.alg.slvr.res      <- kruskal.test(TimeCalc ~ fin.alg.slvr.inters, finRawDF)
fin.alg.conf.slvr.res <- kruskal.test(TimeCalc ~ fin.alg.conf.slvr.inters, finRawDF)

## Find the pairs which are significant
fin.pairs <- pairwise.wilcox.test(finRawDF$TimeCalc, fin.alg.conf.inters,
                                  p.adj="bonf", exact=FALSE, method="holm",
                                  paired=FALSE) %>%
  tidy %>%
  separate(group1, sep=c(3,4), into = c("AlgLeft", "Dump", "ConfigLeft")) %>%
  separate(group2, sep=c(3,4), into = c("AlgRight", "Dump2", "ConfigRight")) %>%
  select(-Dump, -Dump2) %>%
  filter(ConfigRight == ConfigLeft) %>%
  mutate(data = "Financial") %>%
  arrange(p.value)

## We notice here that the p-values are all 1 after the bonferroni adjustment.
## Solver is not statistically significant
fin.slvr.pairs <- pairwise.wilcox.test(finRawDF$TimeCalc, fin.alg.slvr.inters,
                                  p.adj="bonf", exact=TRUE, method="holm",
                                  paired=FALSE) %>%
  tidy %>%
  separate(group1, sep="\\.", into = c("AlgLeft", "SolverLeft")) %>%
  separate(group2, sep="\\.", into = c("AlgRight", "SolverRight")) %>%
  filter(AlgLeft == AlgRight) %>%
  mutate(Significance = case_when(p.value <= 0.05 ~ "Significant",
                                  ... = TRUE ~ "Not Significant")) %>%
  arrange(p.value, SolverLeft)

##################### Auto #############################
## Algorithms are significant
auto.alg.res <- kruskal.test(TimeCalc ~ Algorithm, autoRawDF)

## Versions are significant as expected
auto.vers.res <- kruskal.test(TimeCalc ~ Config, autoRawDF)

## Solvers are actually not significant by themselves
auto.slvr.res <- kruskal.test(TimeCalc ~ DataSet, autoRawDF)

## Interaction bewtween algorithm and version significant as expected
auto.alg.conf.inters      <- interaction(autoRawDF$Algorithm, autoRawDF$Config)
auto.alg.slvr.inters      <- interaction(autoRawDF$Algorithm, autoRawDF$DataSet)
auto.alg.conf.slvr.inters <- interaction(autoRawDF$DataSet, autoRawDF$Algorithm, autoRawDF$Config)

auto.alg.conf.res      <- kruskal.test(TimeCalc ~ auto.alg.conf.inters, autoRawDF)
auto.alg.slvr.res      <- kruskal.test(TimeCalc ~ auto.alg.slvr.inters, autoRawDF)
auto.alg.conf.slvr.res <- kruskal.test(TimeCalc ~ auto.alg.conf.slvr.inters, autoRawDF)

## Autod the pairs which are significant
auto.pairs <- pairwise.wilcox.test(autoRawDF$TimeCalc, auto.alg.conf.inters,
                                  p.adj="bonf", exact=FALSE, method="holm",
                                  paired=FALSE) %>%
  tidy %>%
  separate(group1, sep=c(3,4), into = c("AlgLeft", "Dump", "ConfigLeft")) %>%
  separate(group2, sep=c(3,4), into = c("AlgRight", "Dump2", "ConfigRight")) %>%
  select(-Dump, -Dump2) %>%
  filter(ConfigRight == ConfigLeft) %>%
  mutate(data = "Auto") %>%
  arrange(p.value)

## We notice here that the p-values are all 1 after the bonferroni adjustment.
## Solver is not statistically significant for both datasets
auto.slvr.pairs <- pairwise.wilcox.test(autoRawDF$TimeCalc, auto.alg.slvr.inters,
                                  p.adj="bonf", exact=TRUE, method="holm",
                                  paired=FALSE) %>%
  tidy %>%
  separate(group1, sep="\\.", into = c("AlgLeft", "SolverLeft")) %>%
  separate(group2, sep="\\.", into = c("AlgRight", "SolverRight")) %>%
  filter(AlgLeft == AlgRight) %>%
  mutate(Significance = case_when(p.value <= 0.05 ~ "Significant",
                                  ... = TRUE ~ "Not Significant")) %>%
  arrange(p.value,SolverLeft)

## ########################## Combined data frame ##################
options(scipen = 999)
rq4pvDF <- rbind(auto.pairs, fin.pairs) %>%
  arrange(p.value) %>%
  mutate(Significance = case_when(p.value <= 0.05 ~ "Significant",
                                  TRUE ~ "Not Significant"),
         Version = factor(ConfigLeft, levels =
                                        c("V1", "V2", "V3", "V4", "V5", "V6",
                                          "V7", "V8", "V9", "V10")))

rq4_counts <- rq4pvDF %>% count(AlgLeft, data, Significance)
