library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

finResultsFile <- "../munged_data/financial.csv"
autoResultsFile <- "../munged_data/auto.csv"

finData <- read.csv(file=finResultsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), Mean = Time) %>% select(-Time)

autoData <- read.csv(file=autoResultsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), Mean = Time) %>% select(-Time)

finDF <- finData %>% mutate(data = "Fin")
autoDF <- autoData %>% mutate(data = "Auto")

data <- rbind(finDF, autoDF)

rq1DF <- data %>% filter(ChcCount != 0) %>% group_by(Algorithm) %>%
  arrange(Variants)

rq1DFAuto <- rq1DF %>% filter(data == "Auto")
rq1DFFin <- rq1DF %>% filter(data == "Fin")

#### comparison tables for rq1 rewrite
speedup <- function(new,old) {
  old / new
}

comparison <- function(df) {
  dfComp <- df %>%
    select(Algorithm,DataSet,Variants,Mean) %>%
    pivot_wider(names_from = Algorithm, values_from = Mean)

  colnames(dfComp)[colnames(dfComp) == "v\U27f6v"] <- "vTov"
  colnames(dfComp)[colnames(dfComp) == "v\U27f6p"] <- "vTop"
  colnames(dfComp)[colnames(dfComp) == "p\U27f6v"] <- "pTov"
  colnames(dfComp)[colnames(dfComp) == "p\U27f6p"] <- "pTop"

  dfComp %>% mutate(vTov_pTov = speedup(vTov,pTov),
                    vTov_pTop = speedup(vTov,pTop),
                    vTov_vTop = speedup(vTov,vTop)) %>%
    group_by(DataSet,Variants) %>%
    summarise(vTov_pTov = mean(vTov_pTov),
              vTov_pTop = mean(vTov_pTop),
              vTov_vTop = mean(vTov_vTop))
}

differences <- function(df) {
  dfComp <- df %>%
    select(Algorithm,DataSet,Variants,Mean) %>%
    pivot_wider(names_from = Algorithm, values_from = Mean)

  colnames(dfComp)[colnames(dfComp) == "v\U27f6v"] <- "vTov"
  colnames(dfComp)[colnames(dfComp) == "v\U27f6p"] <- "vTop"
  colnames(dfComp)[colnames(dfComp) == "p\U27f6v"] <- "pTov"
  colnames(dfComp)[colnames(dfComp) == "p\U27f6p"] <- "pTop"

  dfComp %>% mutate(vTov_pTov = (pTov - vTov) / 60,
                    vTov_pTop = (pTop - vTov) / 60,
                    vTov_vTop = (vTop - vTov) / 60) %>%
    select(DataSet,Variants,vTov_pTov,vTov_pTop,vTov_vTop)
}

globalAvgsSpdup <- function(df) {
  ## the global average, i.e., calculate speedup and then average across _all_
  ## variants
  dfComp <- df %>%
    select(Algorithm,DataSet,Variants,Mean) %>%
    pivot_wider(names_from = Algorithm, values_from = Mean)

  colnames(dfComp)[colnames(dfComp) == "v\U27f6v"] <- "vTov"
  colnames(dfComp)[colnames(dfComp) == "v\U27f6p"] <- "vTop"
  colnames(dfComp)[colnames(dfComp) == "p\U27f6v"] <- "pTov"
  colnames(dfComp)[colnames(dfComp) == "p\U27f6p"] <- "pTop"

  dfComp %>% mutate(vTov_pTov = speedup(vTov,pTov),
                    vTov_pTop = speedup(vTov,pTop),
                    vTov_vTop = speedup(vTov,vTop)) %>%
    group_by(DataSet) %>%
    summarise(vTov_pTov = mean(vTov_pTov),
              vTov_pTop = mean(vTov_pTop),
              vTov_vTop = mean(vTov_vTop))
}

rq1CompAuto <- comparison(rq1DFAuto)
rq1CompFin <- comparison(rq1DFFin)

rq1CompAutoDiff <- differences(rq1DFAuto)
rq1CompFinDiff  <- differences(rq1DFFin)

rq1AvgsSpeedupAuto <- globalAvgsSpdup(rq1DFAuto)
rq1AvgsSpeedupFin <- globalAvgsSpdup(rq1DFFin)

rq1CompDiffsByVariantAuto <- rq1CompAutoDiff %>%
  pivot_longer(cols=starts_with("vTov"), names_to = "Comparison", values_to = "Speedup") %>%
  group_by(DataSet,Variants) %>%
  summarise(Speedup = mean(Speedup))

rq1CompDiffsByVariantFin <- rq1CompFinDiff %>%
  pivot_longer(cols=starts_with("vTov"), names_to = "Comparison", values_to = "Speedup") %>%
  group_by(DataSet,Variants) %>%
  summarise(Speedup = mean(Speedup))

rq1CompAvgsByVariantAuto <- rq1CompAuto %>%
  pivot_longer(cols=starts_with("vTov"), names_to = "Comparison", values_to = "Speedup") %>%
  group_by(DataSet,Variants) %>%
  summarise(Speedup = mean(Speedup))

rq1CompAvgsByVariantFin <- rq1CompFin %>%
  pivot_longer(cols=starts_with("vTov"), names_to = "Comparison", values_to = "Speedup") %>%
  group_by(DataSet,Variants) %>%
  summarise(Speedup = mean(Speedup))

rq1CompAvgsByAlgAuto <- rq1CompAuto %>%
  pivot_longer(cols=starts_with("vTov"), names_to = "Comparison", values_to = "Speedup") %>%
  group_by(DataSet,Comparison) %>%
  summarise(Speedup = mean(Speedup))

rq1CompAvgsByAlgFin <- rq1CompFin %>%
  pivot_longer(cols=starts_with("vTov"), names_to = "Comparison", values_to = "Speedup") %>%
  group_by(DataSet,Comparison) %>%
  summarise(Speedup = mean(Speedup))


breaksRq1 <- function(x) {
  if (max(x) < 17) {
    2^(1:4)
  } else {
    2^(5:10)}
  }

rq1AutoZ3 <- ggplot(rq1DFAuto %>% filter(DataSet == "Z3"), aes(x=Variants,y=Mean/60)) +
  geom_line(aes(color=Algorithm)) +
  geom_point(aes(shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  facet_grid(data ~ DataSet, scales = "free_x") +
  theme_classic() +
  scale_x_continuous(breaks=breaksRq1, limits=c(2,NA)) +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = c(0.08,0.75))

rq1FinZ3 <- ggplot(rq1DFFin %>% filter(DataSet == "Z3")) +
  geom_line(aes(x=Variants, y=Mean/60, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean/60, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  facet_grid(data ~ DataSet,scales="free") +
  theme_classic() +
  scale_x_continuous(breaks=breaksRq1, limits=c(2,NA)) +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = c(0.08,0.75))

rq1Auto <- ggplot(rq1DFAuto) +
  geom_line(aes(x=Variants, y=Mean/60, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean/60, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  facet_grid(data ~ DataSet, scales = "free_x") +
  theme_classic() +
  scale_x_continuous(breaks=breaksRq1, limits=c(2,NA)) +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = c(0.08,0.75))

rq1Fin <- ggplot(rq1DFFin) +
  geom_line(aes(x=Variants, y=Mean/60, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean/60, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  facet_grid(data ~ DataSet,scales="free") +
  theme_classic() +
  scale_x_continuous(breaks=breaksRq1, limits=c(2,NA)) +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = c(0.08,0.75), axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1))

## ggsave("../plots/RQ1_AutoZ3.png", plot = rq1AutoZ3, height = 4, width = 7, device = "png")
## ggsave("../plots/RQ1_FinZ3.png", plot = rq1FinZ3, height = 4, width = 7, device = "png")

#### linear models for rq1 discussion
auto.lm.vTov <- lm(Mean ~ Variants, rq1DFAuto %>% filter(Algorithm == "v\U27f6v"))
auto.lm.vTop <- lm(Mean ~ Variants, rq1DFAuto %>% filter(Algorithm == "v\U27f6p"))
auto.lm.pTov <- lm(Mean ~ Variants, rq1DFAuto %>% filter(Algorithm == "p\U27f6v"))
auto.lm.pTop <- lm(Mean ~ Variants, rq1DFAuto %>% filter(Algorithm == "p\U27f6p"))

fin.lm.vTov <- lm(Mean ~ Variants, rq1DFFin %>% filter(Algorithm == "v\U27f6v"))
fin.lm.vTop <- lm(Mean ~ Variants, rq1DFFin %>% filter(Algorithm == "v\U27f6p"))
fin.lm.pTov <- lm(Mean ~ Variants, rq1DFFin %>% filter(Algorithm == "p\U27f6v"))
fin.lm.pTop <- lm(Mean ~ Variants, rq1DFFin %>% filter(Algorithm == "p\U27f6p"))
