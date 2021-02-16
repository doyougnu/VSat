library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

finResultsFile <- "../munged_data/financial.csv"
autoResultsFile <- "../munged_data/auto.csv"
## finRawFile <- "../data/fin_rq3_singletons.csv"
## autoRawFile <- "../data/auto_rq3_singletons.csv"

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

breaksRq1 <- function(x) {
  if (max(x) < 17) {
    2^(1:4)
  } else {
    2^(7:10)}
  }

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

ggsave("../plots/RQ1_Auto.png", plot = rq1Auto, height = 4, width = 7, device = "png")
ggsave("../plots/RQ1_Fin.png", plot = rq1Fin, height = 4, width = 7, device = "png")

### average by solver speedup
speedupByVariantSolver <- function(df) {
  df %>%
    select(DataSet, Algorithm, Variants, Mean) %>%
    group_by(DataSet,Algorithm,Variants) %>%
    summarise(Mean = mean(Mean)) %>%
    mutate(Speedup = lag(Mean, default = first(Mean)) / Mean)
}

speedupBySolver <- function(df) {
  df %>%
    select(DataSet, Algorithm, Variants, Mean) %>%
    group_by(DataSet,Algorithm) %>%
    summarise(Mean = mean(Mean)) %>%
    mutate(Speedup = lag(Mean, default = first(Mean)) / Mean)
}

rq1AutoSpeedupBySolver <- speedupBySolver(rq1DFAuto)
rq1FinSpeedupBySolver  <- speedupBySolver(rq1DFFin)

### average speedup
avgSpeedupFin <- rq1AutoSpeedupBySolver %>%
  group_by(Algorithm) %>%
  summarise(Speedup = mean(Speedup))

avgSpeedupAuto <- rq1FinSpeedupBySolver %>%
  group_by(Algorithm) %>%
  summarise(Speedup = mean(Speedup))
