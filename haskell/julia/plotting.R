library(ggplot2)
library(dplyr)
library(cowplot)
library(latex2exp)
library(gridExtra)
library(forcats)

fin <- "../data/fin_data.csv"
auto <- "../data/auto_data.csv"
finParallel <- "../data/fin_parallel_data.csv"

clean <- function(input_port, name){
  data <- read.csv(file=input_port) %>%
    mutate(Algorithm = as.factor(Algorithm)) %>%
    mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), DataSet = name) %>%
    select(-Name)

  df <- data %>%
    filter(Config != "EvolutionAware") %>%
    mutate(QueryFormulaSize = ChcCount + PlainCount) %>%
    group_by(Algorithm) %>%
    mutate(UniqueDimensions = log2(Variants)
         , VCoreReduction = (QueryFormulaSize - VCoreSize) / QueryFormulaSize)

  return(df)
}

finDf  <- clean(fin, "Financial")
autoDf <- clean(auto, "Auto")
finP <- clean(finParallel, "Financial") %>% mutate(Algorithm = paste(Algorithm, "p", sep=""))

df <- merge(finDf, autoDf, finP, all = TRUE)

## Tomorrow: fix auto data, make facet to show linear vs log scale on x axis where x axis is dimensions and not variants
rq1 <- ggplot(df %>% filter(Variants > 2), mapping = aes(x=Variants, y=Mean, shape = Algorithm, colour=Algorithm)) +
  facet_wrap(. ~ DataSet, scales = "free") +
  geom_point(size=3) +
  geom_line() +
  ylab("Time [s] to solve all Variants") +
  scale_shape_manual(values = c(1,2,5,17)) +
  ggtitle("RQ1: Performance as variants increase") +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing = unit(2, "lines")
        , plot.title = element_text(size=12))

ggsave("../plots/RQ1.png", plot = rq1, device = "png", width = 7, height = 4)


rq3 <- df %>% filter(Variants == 1) %>%
  mutate(VersionNum = as.numeric(gsub("V*", "\\1", Config))) %>%
  arrange(DataSet, Algorithm, VersionNum) %>%
  mutate(Config = fct_reorder(Config, VersionNum)) %>%
  ggplot() +
  # remove axes and superfluous grids
  theme_classic() +
  ggtitle("RQ3: Performance with respect to a single variant") +
  theme(
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(size=12),
        legend.position = "bottom") +

  # add a dummy point for scaling purposes
  geom_point(aes(x = 0.1, y = Config),
             size = 0, col = "white") +

  # add the horizontal discipline lines
  geom_hline(yintercept = 1:10, col = "grey80") +

  # add auto points
  geom_point(aes(x=Mean, y = Config, colour = Algorithm, shape=Algorithm),
             size = 5) +
  scale_shape_manual(values = c(1,2,5,17)) +

  facet_grid(. ~ DataSet, scales="free") +
  xlab("Time [s] to solve a single variant") +
  ylab("Feature Model Version")

ggsave("../plots/RQ3.png", plot = rq3, device = "png", width = 7, height = 4)
