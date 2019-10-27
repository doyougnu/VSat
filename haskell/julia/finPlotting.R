library(ggplot2)
library(dplyr)
library(cowplot)

timingsResultsFile <- "../data/fin_data.csv"

evoFacetLabels <- c( "Sum"
                  , "EvolutionAware"
                  , "V1"
                  , "V2"
                  , "V3"
                  , "V4"
                  , "V5"
                  , "V6"
                  , "V7"
                  , "V8"
                  , "V9"
                  , "V10")

data <- read.csv(file=timingsResultsFile) %>% mutate(Algorithm = as.factor(Algorithm)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

dfCascade <- data %>% filter( Config != "V2"
                      ,Config != "V3"
                      ,Config != "V4"
                      ,Config != "V5"
                      ,Config != "V6"
                      ,Config != "V7"
                      ,Config != "V8"
                      ,Config != "V9"
                      ,Config != "V10"
                      ,Config != "EvolutionAware") %>% filter (Algorithm != "p\U27f6p"
                                                             , Algorithm != "p\U27f6v")

sumData <- data %>% filter( Config != "V1*V2"
                          ,Config != "EvolutionAware"
                          ,Config != "V1*V2*V3"
                          ,Config != "V1*V2*V3*V4"
                          ,Config != "V1*V2*V3*V4*V5"
                          ,Config != "V1*V2*V3*V4*V5*V6"
                          ,Config != "V1*V2*V3*V4*V5*V6*V7"
                          ,Config != "V1*V2*V3*V4*V5*V6*V7*V8"
                          ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9"
                          ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10") %>% group_by(Algorithm) %>%
  summarise(Mean = sum(Mean)) %>% mutate(Config = as.factor("Sum"))

df <- merge(sumData, data, all=TRUE) %>%
  filter( Config != "V1*V2"
         ,Config != "V1*V2*V3"
         ,Config != "V1*V2*V3*V4"
         ,Config != "V1*V2*V3*V4*V5"
         ,Config != "V1*V2*V3*V4*V5*V6"
         ,Config != "V1*V2*V3*V4*V5*V6*V7"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10") %>% mutate(Config = factor(Config, levels = evoFacetLabels))

facetLabels <- c(V1 = "V1"
               , "V1*V2" = "V1**V2"
               , "V1*V2*V3" = "V1**V3"
               , "V1*V2*V3*V4"                    = "V1**V4"
               , "V1*V2*V3*V4*V5"                 = "V1**V5"
               , "V1*V2*V3*V4*V5*V6"              = "V1**V6"
               , "V1*V2*V3*V4*V5*V6*V7"           = "V1**V7"
               , "V1*V2*V3*V4*V5*V6*V7*V8"        = "V1**V8"
               , "V1*V2*V3*V4*V5*V6*V7*V8*V9"     = "V1**V9"
               , "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10" = "V1**V10")

## reorder the facet labels
df$Config <- factor(df$Config, levels=c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "Sum", "EvolutionAware"))

cascade_plt <- ggplot(dfCascade, mapping = aes(x=Algorithm, y=Mean, shape=Algorithm, fill=Algorithm)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Mean [s]") +
  geom_col(position = "dodge") +
  facet_grid(. ~ Config
           , labeller = labeller(Config = facetLabels)
             ) + theme(strip.text.x = element_text(size=10), legend.position = "none")

ggsave("../plots/fin_cascade.png", plot = cascade_plt, device = "png", width = 15, height = 8)

evo_plt <- ggplot(df, mapping = aes(x=Algorithm, y=Mean, shape=Algorithm, fill=Algorithm)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Mean [s]") +
  geom_col(position = "dodge") +
  facet_grid(. ~ Config
           ## , labeller = labeller(Config = evoFacetLabels)
             ) + theme(strip.text.x = element_text(size=10), legend.position   = "none")

ggsave("../plots/fin_evo.png", plot = evo_plt, device = "png")
