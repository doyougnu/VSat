library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)

finResultsFile <- "../data/fin_data.csv"
autoResultsFile <- "../data/auto_data.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

finDF <- finData %>% mutate(data = "Financial") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)
autoDF <- autoData %>% mutate(data = "Auto") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

data <- rbind(finDF, autoDF)

rq1DF <- data %>%
  filter(Variants > 2) %>%
  group_by(Algorithm) %>%
  arrange(Variants) %>%
  mutate(MeanMin = (Mean %% (24 * 3600 * 3600 )) / 60.0)

rq1 <- ggplot(rq1DF, mapping = aes(x=Variants, y=MeanMin, shape=Algorithm, color=Algorithm)) +
  geom_point(size=3) +
  geom_line() +
  scale_shape_manual(values = c(1,2,5,17)) +
  facet_wrap(. ~ data, scales = "free") +
  theme_classic() +
  ggtitle("RQ1: Performance as variants increase") +
  ylab("Time [min] to solve all Variants") +
  theme(legend.position = "bottom")

ggsave("../plots/RQ1.png", plot = rq1, height = 4, width = 7, device = "png")
