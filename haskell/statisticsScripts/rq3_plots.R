library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(latex2exp)
library(broom)
library(scales)
library(ggpubr)

finResultsFile <- "../data/fin_comp_data.csv"
autoResultsFile <- "../data/auto_comp_data.csv"

finData <- read.csv(file=finResultsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config))

autoData <- read.csv(file=autoResultsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config))

finDF <- finData %>% mutate(data = "Fin") %>%
  select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

autoDF <- autoData %>% mutate(data = "Auto") %>%
  select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)


## finding teh sharing ratios for auto
autoPtoP <- autoDF %>% group_by(Algorithm, Config) %>%
  filter(Algorithm == "v-->p" || Algorithm == "p-->p") %>%
  spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->p`)) %>%
  select(-"v-->p", -"p-->p")  %>% mutate(Algorithm = "p\U27f6p")

autoVtoV <- autoDF %>% group_by(Algorithm, Config) %>%
  filter(Algorithm == "v-->p" || Algorithm == "v-->v") %>%
  spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `v-->v`)) %>%
  select(-"v-->v", -"v-->p") %>% mutate(Algorithm = "v\U27f6v")

autoPtoV <- autoDF %>% group_by(Algorithm, Config) %>%
  filter(Algorithm == "v-->p" || Algorithm == "p-->v") %>%
  spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->v`)) %>%
  select(-"v-->p", -"p-->v") %>% mutate(Algorithm = "p\U27f6v")

autoSharedDF <- rbind(autoPtoP, autoVtoV, autoPtoV)

## finding teh sharing ratios for fin
finPtoP <- finDF %>% group_by(Algorithm, Config) %>%
  filter(Algorithm == "v-->p" || Algorithm == "p-->p") %>%
  spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->p`)) %>%
  select(-"v-->p", -"p-->p") %>% mutate(Algorithm = "p\U27f6p")

finVtoV <- finDF %>% group_by(Algorithm, Config) %>%
  filter(Algorithm == "v-->p" || Algorithm == "v-->v") %>%
  spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `v-->v`)) %>%
  select(-"v-->v", -"v-->p") %>% mutate(Algorithm = "v\U27f6v")

finPtoV <- finDF %>% group_by(Algorithm, Config) %>%
  filter(Algorithm == "v-->p" || Algorithm == "p-->v") %>%
  spread(Algorithm, Mean) %>% mutate(MeanRatio = (`v-->p` / `p-->v`)) %>%
  select(-"v-->p", -"p-->v") %>% mutate(Algorithm = "p\U27f6v")

finSharedDF <- rbind(finPtoP, finVtoV, finPtoV)

df <- rbind(autoSharedDF, finSharedDF) %>%
  mutate(PlainRatio = PlainCount / (ChcCount + PlainCount),
         ChcRatio = ChcCount / (ChcCount + PlainCount))

rq2 <- ggplot(df, mapping = aes(x=PlainRatio, y=MeanRatio, colour = Algorithm, shape = Algorithm)) +
  geom_point(size=3, alpha=0.7) +
  ylab("% SpeedUp") +
  xlab(TeX("Plain Ratio:  $\\frac{|Plain Terms|}{|Total Terms|}}")) +
  scale_shape_manual(values = c(1,6,17)) +
  geom_smooth(method=lm, formula = y ~ x, se=TRUE, alpha=0.15) +
  ggtitle("RQ2: Performance as a function of plain ratio") +
  theme_classic() +
  stat_cor(aes(color=Algorithm),
           label.x=0.74, label.y.npc=c(0.91, 0.89, 0.88)) +
  theme(legend.position = c(0.07, 0.83))

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

########## Again violates equal variance and normality ###############

## Algorithms are significant
alg.res <- kruskal.test(MeanRatio ~ Algorithm, df)

## Plain ratio is not significant!!
pr.res <- kruskal.test(MeanRatio ~ PlainRatio, df)

## Interaction is not significant surprisingly
rq2.inters <- interaction(df$Algorithm, df$PlainRatio)
inters.res <- kruskal.test(MeanRatio ~ rq2.inters, df)

## Autod the pairs which are significant
pairs <- pairwise.wilcox.test(df$MeanRatio, df$Algorithm,
                              p.adj="bonf", method="holm"
                            , exact=TRUE, paired=FALSE) %>%
  tidy %>% arrange(p.value)
