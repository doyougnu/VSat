library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(latex2exp)

finResultsFile <- "../data/fin_comp_data.csv"
autoResultsFile <- "../data/auto_comp_data.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config))
autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config))


munge <- function(df) {

dfCascade <- df %>% filter( Config != "V2"
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

sumData <- df %>% filter( Config != "V1*V2"
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

df <- merge(sumData, df, all=TRUE) %>%
  filter( Config != "V1*V2"
         ,Config != "V1*V2*V3"
         ,Config != "V1*V2*V3*V4"
         ,Config != "V1*V2*V3*V4*V5"
         ,Config != "V1*V2*V3*V4*V5*V6"
         ,Config != "V1*V2*V3*V4*V5*V6*V7"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9"
         ,Config != "V1*V2*V3*V4*V5*V6*V7*V8*V9*V10")

dfCascade
}

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


finDF <- munge(finData) %>% mutate(data = "Fin") %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

autoDF <- munge(autoData) %>% mutate(data = "Auto") %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

df <- rbind(finDF,autoDF)  %>% filter(ChcCount != 0) %>% mutate(PlainRatio = PlainCount / (ChcCount + PlainCount), ChcRatio = ChcCount / (ChcCount + PlainCount))

dfMeanRatio <- df %>% group_by(Algorithm, Mean, Config, PlainRatio) %>% spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `v-->v` ))

dfPlainRatio <- df %>% filter(Algorithm == "v-->v")

## model <- lm(log10(Mean) ~ log10(CompressionRatio), df)

## compression_plt <- ggplot(df, mapping = aes(x=CompressionRatio, y=Mean, shape=data, label=Config, color=data, fill=data)) +
##   geom_point(size=3) +
##   ylab("Mean [s]") +
##   ## scale_x_log10() +
##   scale_y_log10() +
##   geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
##   geom_text(nudge_y = 0.1, nudge_x = 0.002, angle = 45, check_overlap = TRUE)


# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

## lm_eqn <- function(df){
##   m <- lm(log10(Mean) ~ log10(CompressionRatio), df);
##   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
##                    list(a = format(unname(coef(m)[1]), digits = 2),
##                         b = format(unname(coef(m)[2]), digits = 2),
##                         r2 = format(summary(m)$r.squared, digits = 3)))
##   as.character(as.expression(eq));
## }


## ggsave("../plots/CompRatio.png", plot = compression_plt, device = "png")

plain_ratio_plt <- ggplot(dfPlainRatio, mapping = aes(x=PlainRatio, y=Mean, label=Config, shape=data, color=data)) + geom_point(size=3) +
  ylab("Mean [s]") +
  ## scale_x_log10() +
  ## scale_y_log10() +
  geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
  geom_text(nudge_y = 0.1, nudge_x = 0.002, angle = 45, check_overlap = TRUE) +
  theme(legend.position = c(.90,.90)) +
  facet_wrap(. ~ data, scales = "free")

  ## theme_cowplot(12)
## evo_plt <- ggplot(df, mapping = aes(x=Algorithm, y=Mean, shape=Algorithm, fill=Algorithm)) +
##   theme(axis.text.x = element_text(angle = 90)) +
##   geom_col(position = "dodge") +
##   facet_grid(. ~ Config
##            ## , labeller = labeller(Config = facetLabels)
##              ) + theme(strip.text.x = element_text(size=10))

## ggsave("../plots/plainRatio.pdf", plot = plain_ratio_plt, device = "pdf")

mean_ratio_plt <- ggplot(dfMeanRatio, mapping = aes(x=PlainRatio, y=MeanRatio, label=Config, color=data)) +
  geom_point(size=3) +
  ylab(TeX("% SpeedUp \t  $\\frac{v \\rightarrow v}{v \\rightarrow p}$")) +
  ## scale_x_log10() +
  ## scale_y_log10() +
  geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
  geom_text(nudge_y = 0.012, nudge_x = 0.003, angle = 45, check_overlap =FALSE) +
  theme(legend.position = c(.85,.90)) +
  ggtitle("RQ2: Performance as a function of plain ratio")
## theme_cowplot(12)
## evo_plt <- ggplot(df, mapping = aes(x=Algorithm, y=Mean, shape=Algorithm, fill=Algorithm)) +
##   theme(axis.text.x = element_text(angle = 90)) +
##   geom_col(position = "dodge") +
##   facet_grid(. ~ Config
##            ## , labeller = labeller(Config = facetLabels)
##              ) + theme(strip.text.x = element_text(size=10))

ggsave("../plots/speedup.pdf", plot = mean_ratio_plt, device = "pdf")
