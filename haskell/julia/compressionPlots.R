library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(latex2exp)

finResultsFile <- "../data/fin_comp_data.csv"
autoResultsFile <- "../data/auto_comp_data.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config))
autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config))

finDF <- finData %>% mutate(data = "Fin") %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)
autoDF <- autoData %>% mutate(data = "Auto") %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)


## finding teh sharing ratios for auto
autoPtoP <- autoDF %>% group_by(Algorithm, Config) %>% filter(Algorithm == "v-->p" || Algorithm == "p-->p") %>% spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->p`)) %>% select(-"v-->p", -"p-->p")  %>% mutate(Algorithm = "p\U27f6p")

autoVtoV <- autoDF %>% group_by(Algorithm, Config) %>% filter(Algorithm == "v-->p" || Algorithm == "v-->v") %>% spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `v-->v`)) %>% select(-"v-->v", -"v-->p") %>% mutate(Algorithm = "v\U27f6v")

autoPtoV <- autoDF %>% group_by(Algorithm, Config) %>% filter(Algorithm == "v-->p" || Algorithm == "p-->v") %>% spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->v`)) %>% select(-"v-->p", -"p-->v") %>% mutate(Algorithm = "p\U27f6v")

autoSharedDF <- rbind(autoPtoP, autoVtoV, autoPtoV)

## finding teh sharing ratios for fin
finPtoP <- finDF %>% group_by(Algorithm, Config) %>% filter(Algorithm == "v-->p" || Algorithm == "p-->p") %>% spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->p`)) %>% select(-"v-->p", -"p-->p") %>% mutate(Algorithm = "p\U27f6p")

finVtoV <- finDF %>% group_by(Algorithm, Config) %>% filter(Algorithm == "v-->p" || Algorithm == "v-->v") %>% spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `v-->v`)) %>% select(-"v-->v", -"v-->p") %>% mutate(Algorithm = "v\U27f6v")

finPtoV <- finDF %>% group_by(Algorithm, Config) %>% filter(Algorithm == "v-->p" || Algorithm == "p-->v") %>% spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->v`)) %>% select(-"v-->p", -"p-->v") %>% mutate(Algorithm = "p\U27f6v")

finSharedDF <- rbind(finPtoP, finVtoV, finPtoV)

## df <- rbind(finDF,autoDF)  %>% filter(ChcCount != 0) %>%
df <- rbind(autoSharedDF, finSharedDF) %>% mutate(PlainRatio = PlainCount / (ChcCount + PlainCount), ChcRatio = ChcCount / (ChcCount + PlainCount))

## dfMeanRatio <- df %>% filter(Algorithm = "v-->v")spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `v-->v` ))

## dfPlainRatio <- df %>% filter(Algorithm == "v-->v" || Algorithm = "v-->p")

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

## plain_ratio_plt <- ggplot(dfPlainRatio, mapping = aes(x=PlainRatio, y=Mean, label=Config, shape=data, color=data)) + geom_point(size=3) +
##   ylab("Mean [s]") +
##   ## scale_x_log10() +
##   ## scale_y_log10() +
##   geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
##   geom_text(nudge_y = 0.1, nudge_x = 0.002, angle = 45, check_overlap = TRUE) +
##   theme(legend.position = c(.90,.90)) +
##   facet_wrap(. ~ data, scales = "free")

  ## theme_cowplot(12)
## evo_plt <- ggplot(df, mapping = aes(x=Algorithm, y=Mean, shape=Algorithm, fill=Algorithm)) +
##   theme(axis.text.x = element_text(angle = 90)) +
##   geom_col(position = "dodge") +
##   facet_grid(. ~ Config
##            ## , labeller = labeller(Config = facetLabels)
##              ) + theme(strip.text.x = element_text(size=10))

## ggsave("../plots/plainRatio.pdf", plot = plain_ratio_plt, device = "pdf")

rq2 <- ggplot(df, mapping = aes(x=PlainRatio, y=MeanRatio, colour = Algorithm)) +
  geom_point(size=3, aes(shape = Algorithm)) +
  ## ylab(TeX("% SpeedUp \t  $\\frac{v \\rightarrow v}{v \\rightarrow p}$")) +
  ylab("% SpeedUp") +
  xlab(TeX("Plain Ratio:  $\\frac{|Plain Terms|}{|Total Terms|}}")) +
  ## scale_x_log10() +
  ## scale_y_log10() +
  geom_smooth(method=lm, formula = y ~ x, se=FALSE) +
  ## geom_text(nudge_y = 0.012, nudge_x = 0.003, angle = 45, check_overlap =FALSE) +
  theme(legend.position = c(.85,.90)
      , plot.title = element_text(size=12)) +
  ggtitle("RQ2: Performance as a function of plain ratio") +
  theme_classic()

ggsave("../plots/RQ2.png", plot = rq2, device = "png", height = 4, width = 7)
