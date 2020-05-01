library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)

finResultsFile <- "../data/fin_dead_core_data.csv"
autoResultsFile <- "../data/auto_dead_core_data.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

finDF <- finData %>% mutate(data = "Fin") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)
autoDF <- autoData %>% mutate(data = "Auto") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

data <- rbind(finDF, autoDF)

deadCoreDF <- data %>% filter(Variants > 2) %>% group_by(Algorithm) %>% arrange(Variants)

dc <- ggplot(rq1DF, mapping = aes(x=Variants, y=Mean, shape=Algorithm, color=Algorithm)) +
  ylab("Mean [s]") +
  geom_point(size=3) +
  geom_line() +
  scale_shape_manual(values = c(1,2,5,17)) +
  facet_wrap(. ~ data, scales = "free") +
  theme_classic() +
  ggtitle("Dead Core Demonstration") +
  ylab("Time [s] to solve all Variants") +
  theme(legend.position = "bottom")

ggsave("../plots/DeadCore.png", plot = dc, height = 4, width = 7, device = "png")
