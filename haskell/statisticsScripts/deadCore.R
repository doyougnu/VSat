library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)

finResultsFile <- "../data/fin_dead_core.csv"
autoResultsFile <- "../data/auto_dead_core.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

finDF <- finData %>% mutate(data = "Fin")
autoDF <- autoData %>% mutate(data = "Auto")

data <- rbind(finDF, autoDF)

deadCoreDF <- data %>%
  group_by(data) %>%
  arrange(Mean) %>%
  mutate(MeanLbl = case_when(data == "Auto" ~ signif(Mean, 5),
                             data == "Fin" ~ signif(Mean, 3)))

dc <- ggplot(deadCoreDF
           , mapping = aes(x=Algorithm
                         , y=Mean
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
       y = "Time [s] to solve Dead and Core Analysis over all Versions") +
  theme(legend.position = "none") +
  coord_flip()

ggsave("../plots/DeadCore.png", plot = dc, height = 4, width = 7, device = "png")

deadCoreDF %>% select(data,Algorithm, Mean) %>% group_by(data) %>%
  spread(Algorithm, Mean) %>% mutate(speedup = `v⟶p` / `v⟶v`)
