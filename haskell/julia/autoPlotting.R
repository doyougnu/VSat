library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)

timingsResultsFile <- "../data/auto_data.csv"

data <- read.csv(file=timingsResultsFile) %>%
  filter(Config != "EvolutionAware") %>%
  mutate(Algorithm = as.factor(Algorithm)
       , Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm)) %>% select(-Name)

## df <- merge(sumData, data, all=TRUE) %>%
singleton_means <- data %>%
  filter( Config != "V1*V2"
         ,Config != "V1*V2*V3"
         ,Config != "V1*V2*V3*V4"
         ,Config != "V1*V2*V3*V4*V5"
         ,Config != "V1*V2*V3*V4*V5*V6"
         ,Config != "V1*V2*V3*V4*V5*V6*V7"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10"
         ,Config != "EvolutionAware") %>%
  group_by(Algorithm) %>%
  summarise(SingletonMean = mean(Mean))

df <- merge(singleton_means,data, all=TRUE) %>%
  group_by(Algorithm) %>%
  mutate(NormPerf = Mean / SingletonMean) %>%
  drop_na()


cascade_plt <- ggplot(df, mapping = aes(x=Variants, y=NormPerf, shape=Algorithm, fill=Algorithm)) +
  ylab("Normalized Time") +
  geom_point(size=3) +
  geom_line() +
  scale_shape_manual(values = c(1,2,5,17))

## ggsave("../plots/auto_cascade.png", plot = cascade_plt, device = "png")
