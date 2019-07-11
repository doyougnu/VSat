library(ggplot2)
library(dplyr)

timingsResultsFile <- "../data/auto_data.csv"

data <- read.csv(file=timingsResultsFile)

df <- data %>% filter( Config != "V2"
                      ,Config != "V3"
                      ,Config != "V4"
                      ,Config != "V5"
                      ,Config != "V6"
                      ,Config != "V7"
                      ,Config != "V8"
                      ,Config != "V9"
                      ,Config != "V10"
                      ,Config != "EvolutionAware") %>% filter (Algorithm != "p-->p"
                                                             , Algorithm != "p-->v")

## df <- data %>% filter( Config != "V1*V2"
##                       ,Config != "V1*V2*V3"
##                       ,Config != "V1*V2*V3*V4"
##                       ,Config != "V1*V2*V3*V4*V5"
##                       ,Config != "V1*V2*V3*V4*V5*V6"
##                       ,Config != "V1*V2*V3*V4*V5*V6*V7"
##                       ,Config != "V1*V2*V3*V4*V5*V6*V7*V8"
##                       ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9"
##                       ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10")

facetLabels <- c(V1 = "V1"
               , "V1*V2" = "V1**V2"
               , "V1*V2*V3" = "V1...**...V3"
               , "V1*V2*V3*V4"                    = "V1...**...V4"
               , "V1*V2*V3*V4*V5"                 = "V1...**...V5"
               , "V1*V2*V3*V4*V5*V6"              = "V1...**...V6"
               , "V1*V2*V3*V4*V5*V6*V7"           = "V1...**...V7"
               , "V1*V2*V3*V4*V5*V6*V7*V8"        = "V1...**...V8"
               , "V1*V2*V3*V4*V5*V6*V7*V8*V9"     = "V1...**...V9"
               , "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" = "V1...**...V10")

ggplot(df, mapping = aes(x=Algorithm, y=Mean, shape=Algorithm, fill=Algorithm)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ## scale_y_log10() +
  ## scale_x_log10() +
  ## geom_point() + geom_smooth(method = lm, se = FALSE)
  geom_col(position = "dodge") +
  facet_grid(. ~ Config, labeller = labeller(Config = facetLabels)) + theme(strip.text.x = element_text(size=10))

ggsave("../plots/fin_cascade.png",device = "png")
