library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)

finResultsFile <- "../data/fin_dead_core_data.csv"
autoResultsFile <- "../data/auto_dead_core_data.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

finDF <- finData %>% mutate(data = "Financial") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)
autoDF <- autoData %>% mutate(data = "Auto") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

data <- rbind(finDF, autoDF)

deadCoreDF <- data %>%
  group_by(data) %>%
  arrange(Mean) %>%
  mutate(MeanMin = (Mean %% (24 * 3600 * 3600 )) / 60.0,
         MeanLbl = case_when(data == "Auto" ~ signif(MeanMin, 3),
                             data == "Financial" ~ signif(MeanMin, 3)))


dc <- ggplot(deadCoreDF
           , mapping = aes(x=Algorithm
                         , y=MeanMin
                         , color = Algorithm)) +
  theme_classic() +
  guides(fill = FALSE) +
  guides(size = FALSE) +
  guides(color = FALSE) +
  geom_bar(stat="identity", fill="white", width=0.5
           ,size=1.15) +
  geom_text(aes(label=MeanLbl, hjust=1.4), color="black") +
  facet_wrap(. ~ data, scales = "free_x") +
  labs(title = "Dead Core Demonstration",
       y = "Time [min] to solve Dead and Core Analysis over all Versions") +
  theme(legend.position = "none") +
  coord_flip()

ggsave("../plots/DeadCore.png", plot = dc, height = 4, width = 7, device = "png")

## dcAuto <- ggplot(deadCoreDF %>% filter(data =="Auto"), mapping = aes(x=Algorithm, y=Mean, shape=Algorithm, color=Algorithm)) +
##   ylab("Mean [s]") +
##   geom_point(size=3) +
##   geom_line() +
##   scale_shape_manual(values = c(1,2,5,17)) +
##   scale_x_continuous(breaks=seq(0, 32, 8)) +
##   theme_classic() +
##   ggtitle("Dead Core Demonstration") +
##   ylab("Time [s] to solve all Variants") +
##   theme(legend.position = "bottom")
