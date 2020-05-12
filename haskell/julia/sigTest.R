library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)
library(Hmisc)
library(broom)
library(ggpubr)
library(scales)
library(rstatix)

finRawFile <- "../data/fin_rq3_singletons.csv"
autoRawFile <- "../data/auto_rq3_singletons.csv"

################# Singleton Analysis ##############################

finSingData <- read.csv(file=finRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Financial") %>%
  group_by(Algorithm, Config) %>%
  mutate(TimeCalc = time -append(0,head(time, -1))) %>% filter(TimeCalc > 0)


autoSingData <- read.csv(file=autoRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Auto") %>%
  group_by(Algorithm, Config) %>%
  mutate(TimeCalc = time -append(0,head(time, -1))) %>% filter(TimeCalc > 0)


##################### Financial #############################
## Algorithms are significant
fin.alg.res <- kruskal.test(TimeCalc ~ Algorithm, finSingData)

## Versions are significant as expected
fin.vers.res <- kruskal.test(TimeCalc ~ Config, finSingData)

## Interaction, also significant as expected
fin.inters <- interaction(finSingData$Algorithm, finSingData$Config)
fin.inters.res <- kruskal.test(TimeCalc ~ fin.inters, finSingData)

## Find the pairs which are significant
fin.pairs <- pairwise.wilcox.test(finSingData$TimeCalc, fin.inters,
                                  method="holm", exact=FALSE, paired=FALSE) %>%
  tidy %>%
  separate(group1, sep=c(3,4), into = c("AlgLeft", "Dump", "ConfigLeft")) %>%
  separate(group2, sep=c(3,4), into = c("AlgRight", "Dump2", "ConfigRight")) %>%
  select(-Dump, -Dump2) %>%
  filter(ConfigRight == ConfigLeft) %>%
  mutate(data = "Financial") %>%
  arrange(p.value)

##################### Auto #############################
## Algorithms are significant
auto.alg.res <- kruskal.test(TimeCalc ~ Algorithm, autoSingData)

## ## Versions are significant as expected
auto.vers.res <- kruskal.test(TimeCalc ~ Config, autoSingData)

## ## Interaction, also significant as expected
auto.inters <- interaction(autoSingData$Algorithm, autoSingData$Config)
auto.inters.res <- kruskal.test(TimeCalc ~ auto.inters, autoSingData)

## ## Autod the pairs which are significant
auto.pairs <- pairwise.wilcox.test(autoSingData$TimeCalc, auto.inters,
                                   p.adj="bonf", method="holm"
                                 , exact=TRUE, paired=FALSE) %>%
  tidy %>%
              separate(group1, sep=c(3,4),
                       into = c("AlgLeft", "Dump", "ConfigLeft")) %>%
              separate(group2, sep=c(3,4),
                       into = c("AlgRight", "Dump2", "ConfigRight")) %>%
             select(-Dump, -Dump2) %>%
             filter(ConfigRight == ConfigLeft) %>%
             mutate(data = "Auto") %>%
             arrange(p.value)


########################## Combined data frame ##################
options(scipen = 999)
rq3pvDF <- rbind(auto.pairs, fin.pairs) %>%
  arrange(p.value) %>%
  mutate(Significance = case_when(p.value <= 0.05 ~ "Significant",
                                  TRUE ~ "Not Significant"),
         Version = factor(ConfigLeft, levels =
                                        c("V1", "V2", "V3", "V4", "V5", "V6",
                                          "V7", "V8", "V9", "V10")))

### It seems strange given rq3 plot that only v1 is significant for auto by
### algorithms given that the averages between groups are so different. This is
### because there is a high degree of variance between the samples, you can
### observe this here:

## ggplot(autoSingData, aes( x = Config, y = TimeCalc, color=Algorithm)) + geom_boxplot() + geom_jitter()
