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

finFile <- "../data/fin_diag.csv"
autoFile <- "../data/auto_diag.csv"
finCountsFile <- "../data/fin_counts.csv"
autoCountsFile <- "../data/auto_counts.csv"

finCountData  <- read.csv(file=finCountsFile) %>% mutate(data = "Fin")
autoCountData <- read.csv(file=autoCountsFile) %>% mutate(data = "Auto")

autoRatio <- autoCountData %>%
  group_by(Variants) %>%
  count(Satisfiable) %>%
  pivot_wider(names_from=Satisfiable, values_from=n) %>%
  mutate(UnSatRatio = UnSat / (UnSat + Sat),
         data="Auto") %>%
  replace_na(list(UnSatRatio = 0, UnSat = 0))

finRatio <- finCountData %>%
  group_by(Variants) %>%
  count(Satisfiable) %>%
  pivot_wider(names_from=Satisfiable, values_from=n) %>%
  mutate(UnSatRatio = signif(UnSat / (UnSat + Sat), 3),
         data = "Fin") %>%
  replace_na(list(UnSatRatio = 0, UnSat = 0))

finDF <- read.csv(file=finFile) %>% mutate(data = "Fin", UnchangedRatio = NumUnchange /NumFeatures ) %>% merge(finRatio)
autoDF <- read.csv(file=autoFile) %>% mutate(data = "Auto", UnchangedRatio = NumUnchange / NumFeatures) %>% merge(autoRatio)

df <- rbind(finDF, autoDF)

breaksRq1 <- function(x) {
  if (max(x) > 16) {
    2^(1:10)
  } else {
    2^(1:4)}
}
p <- ggplot(df, aes(x=Variants)) +
  geom_point(aes(y=UnchangedRatio,color="% Features Unchanged"),size=2) +
  geom_line(aes(y=UnchangedRatio, color="% Features Unchanged"),
            linetype="dashed", size=1.1) +
  geom_point(aes(y=UnSatRatio,color="% Unsatisfiable Models"),size=2) +
  geom_line(aes(y=UnSatRatio, color="% Unsatisfiable Models"),
            linetype="dotdash", size=1.1) +
  ## scale_x_log10() +
  facet_wrap(. ~ data, scales="free_x") +
  theme_classic() +
  scale_x_log10(breaks=breaksRq1) +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  theme(legend.position=c(0.80,0.15),
        legend.text=element_text(size=10),
        legend.key.size = unit(.55,'cm')) +
  guides(color=guide_legend("")) +
  ylab("Percent of Total") +
  ggtitle("Ratio of unsatisfiable variants, and constant Features in V-Model") +

ggsave("../plots/VModel.png", plot = p, height = 4, width = 7, device = "png")
