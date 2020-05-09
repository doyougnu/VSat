library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(latex2exp)
library(broom)
library(scales)

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


rq2 <- ggplot(df, mapping = aes(x=PlainRatio, y=MeanRatio, colour = Algorithm, shape = Algorithm)) +
  geom_point(size=3, alpha=0.7) +
  ## ylab(TeX("% SpeedUp \t  $\\frac{v \\rightarrow v}{v \\rightarrow p}$")) +
  ylab("% SpeedUp") +
  xlab(TeX("Plain Ratio:  $\\frac{|Plain Terms|}{|Total Terms|}}")) +
  scale_shape_manual(values = c(1,6,17)) +
  geom_smooth(method=lm, formula = y ~ x, se=TRUE, alpha=0.15) +
  ggtitle("RQ2: Performance as a function of plain ratio") +
  theme_classic() +
  stat_cor(aes(color=Algorithm),
           label.x=0.74, label.y.npc=c(0.91, 0.89, 0.88)) +
  theme(legend.position = c(0.07, 0.83))

## rq22 <- ggscatter(df, x="PlainRatio", y="MeanRatio", color="Algorithm",
##                   palette="jco", size=3, alpha=0.7, ggtheme=theme_bw()
##                   , add="reg.line", shape="Algorithm", conf.int = TRUE) +
##   scale_shape_manual(values = c(1,2,17))

ggsave("../plots/RQ2.png", plot = rq2, device = "png", height = 4, width = 7)


### Perform the anova
res.aov <- aov(MeanRatio ~ PlainRatio * Algorithm, data = df)

### Check the summary to see what is significant, all of it is as expected
res.sig <- summary(res.aov)

### Autoally, perform the pair-wise Tukey comparison to test the difference
### between groups
tuk_res <- TukeyHSD(res.aov) %>% tidy %>% mutate(pVal = scientific(adj.p.value, 3))

res.ass <- plot(res.aov, 2)

aov.resids <- residuals(object=res.aov)

## Shapiro-Wilk normality test
res.shaps <- shapiro.test(x = aov.resids)
