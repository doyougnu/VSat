library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

finSingletonsFile  <- "../munged_data/financial_singletons.csv"
autoSingletonsFile <- "../munged_data/auto_singletons.csv"

finData <- read.csv(file=finSingletonsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(data = "Fin", Algorithm = gsub("-->", "\U27f6", Algorithm), Mean = Time) %>% select(-Time)


autoData <- read.csv(file=autoSingletonsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(data = "Auto", Algorithm = gsub("-->", "\U27f6", Algorithm), Mean = Time) %>% select(-Time)


data <- rbind(finData, autoData)

################# Singleton  Plots ##############################
## Need to filter out version variants which have choices for rq1
rq3DF <- data %>% filter(ChcCount == 0) %>%
  mutate(plotOrdering = as.numeric(substring(Config, 2))) %>%
  mutate(Config = factor(Config, levels = c("V1", "V2", "V3", "V4", "V5", "V6",
                                            "V7", "V8", "V9", "V10")))

## ## custom breaks for the facets
breaksRq3 <- function(x) {
  if (max(x) < 4) {
    ## then we are in fin, NA to just have R find the max
    seq(0, 1.2, 0.10)
  } else {
    seq(0, 150, 10)
  }
}

rq3 <- function(df) {
  ggplot(df, aes(x=Config, y=Mean, fill=Algorithm, shape=Algorithm, color=Algorithm)) +
    geom_point(size=6) +
    scale_shape_manual(values = c(1,6,5,17)) +
    theme_classic() +
    scale_y_continuous(limits=c(0, NA), breaks=breaksRq3) +
    facet_wrap(DataSet ~ .) +
    ## stat_summary(fun.data="mean_sdl"
    ##            , fun.args = list(mult=2)
    ##            , geom="pointrange"
    ##            , color="black"
    ##            , size=0.65) +
    ggtitle("RQ3: Overhead of Variational Solving on Plain Formulas") +
    ylab("Time [s] to solve single version variant") +
    xlab("Feature Model Version") +
    theme(legend.position = c(0.42,0.75),
          legend.key.size = unit(.65,'cm')) +
    theme(panel.grid.major.y = element_line(color = "grey")) +
    coord_flip()
}

rq3_auto <- rq3(rq3DF %>% filter(data == "Auto"))
rq3_fin  <- rq3(rq3DF %>% filter(data == "Fin"))

## ## ggsave("../plots/RQ3.png", plot = rq3, height = 4, width = 7, device = "png")


slow_down <- rq3DF %>% group_by(data,DataSet,Algorithm) %>%  summarise(AvgMean = mean(Mean))
