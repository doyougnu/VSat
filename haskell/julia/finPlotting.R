library(ggplot2)
library(dplyr)
library(cowplot)
library(latex2exp)

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
                      ,Config != "EvolutionAware") %>% mutate(QueryFormulaSize = ChcCount + PlainCount)

## %>% filter (Algorithm != "p\U27f6p"
##           , Algorithm != "p\U27f6v")

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

cascade_plt <- ggplot(dfCascade, mapping = aes(x=Variants, y=Mean, shape=Algorithm,colour=Algorithm)) +
  ## theme(axis.text.x = element_text(angle = 90)) +
  ylab("Mean [s]") +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,5,17)) +
  geom_line(aes(linetype=Algorithm)) +
  scale_x_continuous(breaks = seq(0, 130,10)) +
  ggtitle("RQ1: Performance as variants represented by a query formula increases")

ggsave("../plots/perf_by_variation.png", plot = cascade_plt, device = "png", width = 15, height = 8)

perf_by_vcore_reduction <- ggplot(dfCascade, mapping = aes(x=VCoreSize/QueryFormulaSize, y=Mean, shape=Algorithm,colour=Algorithm)) +
  ## theme(axis.text.x = element_text(angle = 90)) +
  ylab("Mean [s]") +
  xlab(TeX("Percent reduction of Query formula done by accumulation/evaluation:   $\\frac{|Variational Core|}{|Query Formula|}$")) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,5,17)) +
  geom_line(aes(linetype=Algorithm))

ggsave("../plots/perf_by_vcore_reduction.png", plot = perf_by_vcore_reduction, device = "png", width = 15, height = 8)
