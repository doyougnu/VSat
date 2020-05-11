library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)
library(Hmisc)
library(broom)
library(ggpubr)
library(scales)

finResultsFile <- "../data/fin_data.csv"
autoResultsFile <- "../data/auto_data.csv"
finRawFile <- "../data/fin_rq3_singletons.csv"
autoRawFile <- "../data/auto_rq3_singletons.csv"
finCountsFile <- "../data/fin_counts.csv"
autoCountsFile <- "../data/auto_counts.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))
autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

finDF <- finData %>% mutate(data = "Financial") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)
autoDF <- autoData %>% mutate(data = "Auto") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

finCountData  <- read.csv(file=finCountsFile) %>% mutate(data = "Financial")
autoCountData <- read.csv(file=autoCountsFile) %>% mutate(data = "Auto")

autoRatio <- autoCountData %>%
  group_by(Variants) %>%
  count(Satisfiable) %>%
  pivot_wider(names_from=Satisfiable, values_from=n) %>%
  mutate(UnSatRatio = UnSat / (UnSat + Sat)) %>%
  replace_na(list(UnSatRatio = 0, UnSat = 0))

finRatio <- finCountData %>%
  group_by(Variants) %>%
  count(Satisfiable) %>%
  pivot_wider(names_from=Satisfiable, values_from=n) %>%
  mutate(UnSatRatio = signif(UnSat / (UnSat + Sat), 3)) %>%
  replace_na(list(UnSatRatio = 0, UnSat = 0))

data <- rbind(finDF, autoDF)
rq1DF <- data %>% filter(Variants >= 2) %>% group_by(Algorithm) %>% arrange(Variants)

breaks <- function(x) {
  if (max(x) > 16) {
    2^(1:10)
  } else {
    2^(1:4)}
  }

rq1 <- ggplot(rq1DF) +
  geom_line(aes(x=Variants, y=Mean/60, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean/60, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  scale_x_continuous(breaks=breaks, limits=c(2,NA)) +
  facet_wrap(. ~ data, scales = "free") +
  theme_classic() +
  ggtitle("RQ1: Performance as variants increase") +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = c(0.6,0.75))
  ## theme(axis.title.x = element_blank(),axis.text.x = element_blank(),
  ##       axis.line.x = element_blank(), axis.ticks.x = element_blank())

## rq1AutoBottom <- ggplot(autoCountData, aes(x=Variants)) +
##   geom_bar(aes(fill=Satisfiable), stat="count", width=0.5) +
##   geom_text(data=autoRatio, aes(label=paste(UnSatRatio, "%"), angle=90, y=20)) +
##   ## stat_bin(aes()),
##   ##          geom="text", position="identity") +
##   theme_classic() +
##   theme(legend.position = "none") +
##   scale_y_continuous(expand=c(0.3,0))

## rq1FinBottom <- ggplot(finCountData, aes(x=Variants)) +
##   geom_bar(aes(fill=Satisfiable), stat="count", width=40) +
##   geom_text(data=finRatio
##           , aes(label=paste(UnSatRatio, "%"), angle=90, y=1300)
##           , position=position_dodge(width=10)) +
##   theme_classic() +
##   theme(axis.title.y = element_blank()) +
##   scale_y_continuous(expand=c(0.3,0))

## legend1 <- get_legend(rq1FinBottom)
## legend2 <- get_legend(rq1Top)

## rq1 <- ggarrange(rq1Top,
##                  ggarrange(rq1AutoBottom, rq1FinBottom, common.legend=TRUE, legend="bottom"),
##                  ncol=1,
##                  common.legend=TRUE,
##                  legend = "right",
##                  align="hv"
##                  ## heights = c(4,2)
##                  )

ggsave("../plots/RQ1.png", plot = rq1, height = 4, width = 7, device = "png")

################# Singleton Analysis ##############################

rq3DF <- data %>% filter(Variants <= 2) %>%
  mutate(plotOrdering = as.numeric(substring(Config, 2))) %>%
  mutate(Config = factor(Config, levels = c("V1", "V2", "V3", "V4", "V5", "V6",
                                            "V7", "V8", "V9", "V10")))

rq3 <- ggplot(rq3DF, aes(x=Config, y=Mean, fill=Algorithm, shape=Algorithm, color=Algorithm)) +
  geom_point(size=6) +
  scale_shape_manual(values = c(1,6,5,17)) +
  theme_classic() +
  ## scale_y_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  facet_wrap(.~ data, scales="free") +
  ## stat_summary(fun.data="mean_sdl"
  ##            , fun.args = list(mult=2)
  ##              , geom="pointrange"
  ##              , color="black"
  ##              , size=0.65) +
  ggtitle("RQ3: Overhead of Variational Solving on Plain Formulas") +
  ylab("Time [s] to solve single version variant") +
  xlab("Feature Model Version") +
  theme(legend.position = "bottom") +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  coord_flip()

## ggsave("../plots/RQ3.png", plot = rq3, height = 4, width = 7, device = "png")

slow_down <- rq3DF %>% group_by(data,Algorithm) %>%  summarise(AvgMean = mean(Mean))

finSingData <- read.csv(file=finRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Financial") %>%
  group_by(Algorithm, Config) %>%
  mutate(TimeCalc = time -append(0,head(time, -1))) %>% filter(TimeCalc > 0)


autoSingData <- read.csv(file=autoRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Auto") %>%
  group_by(Algorithm, Config) %>%
  mutate(TimeCalc = time -append(0,head(time, -1))) %>% filter(TimeCalc > 0)

### Do a two-way anova looking at both algorithm and config and their interaction
res.fin.aov <- aov(TimeCalc ~ Algorithm * Config, data = finSingData)

### Check the summary to see what is significant, all of it is as expected
res.fin.sig <- summary(res.fin.aov)

### Finally, perform the pair-wise Tukey comparison to test the difference
### between groups
fin.tuk_res <- TukeyHSD(res.fin.aov, which = "Algorithm:Config") %>%
  tidy %>%
  ### cleanup
  separate(comparison, sep=c(3, 4, 7, 11)
         , into = c("AlgLeft", "Dump", "ConfigLeft", "AlgRight", "ConfigRight")) %>%
  mutate(ConfigLeft = gsub("-", "", ConfigLeft),
         ConfigRight = gsub(":","", ConfigRight),
         AlgRight = gsub(":|-", "", AlgRight),
         data = "Financial",
         pVal = scientific(adj.p.value, 3)) %>%
  select(-Dump) %>%
  ## remove out-group comparisons between different variants, e.g., V1 - V10
  filter(ConfigLeft == ConfigRight) %>%
  mutate(Comparison=paste(AlgLeft,":",AlgRight,":",ConfigLeft, sep=""),
         AlgComparison=paste(AlgLeft,":",AlgRight, sep="")) %>%
  ## sort by p-value
  arrange(adj.p.value)

### Check leverage of the observations for outliers
## res.fin.outliers <- plot(res.fin.aov, 1)
### observations 251, 148 and 307 are severe outliers for this data


### Check the normality assumption of the ANOVA
## res.fin.ass <- plot(res.fin.aov, 2)

## check residuals
aov.fin.resids <- residuals(object=res.fin.aov)

## perform Shapiro-wilk normality test
res.fin.shaps <- shapiro.test(x = aov.fin.resids)

## Shapiro-Wilk normality test

## data:  aov_resids
## W = 0.87101, p-value < 2.2e-16

### those outliers are too significant!!!

############################# Auto ANOVA ##############################
### Do a two-way anova looking at both algorithm and config and their interaction
res.auto.aov <- aov(TimeCalc ~ Algorithm * Config, data = autoSingData)

### Check the summary to see what is significant, all of it is as expected
res.auto.sig <- summary(res.auto.aov)

### Autoally, perform the pair-wise Tukey comparison to test the difference
### between groups
auto.tuk_res <- TukeyHSD(res.auto.aov, which = "Algorithm:Config") %>%
  tidy %>%
### cleanup
  separate(comparison, sep=c(3, 4, 7, 11)
         , into = c("AlgLeft", "Dump", "ConfigLeft", "AlgRight", "ConfigRight")) %>%
  mutate(ConfigLeft = gsub("-", "", ConfigLeft),
         ConfigRight = gsub(":","", ConfigRight),
         AlgRight = gsub(":|-", "", AlgRight),
         data = "Auto",
         pVal = scientific(adj.p.value, 3),
         Comparison=paste(AlgLeft,":",AlgRight,":",ConfigLeft, sep=""),
         AlgComparison=paste(AlgLeft,":",AlgRight, sep="")) %>%
  select(-Dump) %>%
  ## remove out-group comparisons between different variants, e.g., V1 - V10
  filter(ConfigLeft == ConfigRight) %>%
  ## sort by p-value
  arrange(adj.p.value)


### Check leverage of the observations for outliers
## res.auto.outliers <- plot(res.auto.aov, 1)


### Check the normality assumption of the ANOVA
res.auto.ass <- plot(res.auto.aov, 2)

aov.auto.resids <- residuals(object=res.auto.aov)

## Shapiro-Wilk normality test
res.auto.shaps <- shapiro.test(x = aov.auto.resids)


########################## Plot p-values ##################
options(scipen = 999)
rq3pvDF <- rbind(auto.tuk_res, fin.tuk_res) %>%
  arrange(pVal) %>%
  mutate(Significance = case_when(adj.p.value <= 0.05 ~ "Significant",
                                  TRUE ~ "Not Significant"),
         SigLabel = case_when(Significance == "Significant" ~ Comparison,
                              TRUE ~ ""),
         SigColor = paste(AlgLeft,":",Significance,sep=""),
         Version = factor(ConfigLeft, levels = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")))



## fin.pval.plt <- ggdotchart(rq3pvDF %>% filter(Significance == "Significant"),
##                            x="ConfigLeft", y="AlgComparison",
##                            shape="AlgLeft",
##                            dot.size=3, label="pVal", fill="adj.p.value",
##                            font.label = list(rotate=TRUE, color ="AlgLeft" , vjust=2,rotate=TRUE),
##                            ggtheme=theme_pubr(), y.text.col=TRUE) +
##   facet_wrap(. ~ data, scales = "free") +
##   theme_classic() +
##   ## theme(legend.position="none") +
##   theme(panel.grid.major.y = element_line(color = "grey")) +
##   scale_shape_manual(values = c(2,5,17))


pval.plt <- ggplot(rq3pvDF, aes(x=AlgLeft, y=AlgRight, size=(1-adj.p.value),
                                shape=SigColor, color=Significance)) +
  geom_point() +
  geom_jitter() +
  ## geom_text(size=5) +
  ## geom_tile()
  ## scale_shape_manual(values = c(6,5,2)) +
  ## scale_color_manual
  theme_classic() +
  facet_grid(data ~ Version, scales="free") +
  scale_size_continuous(range=c(2,4)) +
  scale_shape_manual(values = c(6,5,18,2,17)) +
  ## scale_shape_discrete(name = "Algorithm",
  ##                      labels=c("p\U27f6v","v\U27f6p", "v\U27f6v", "", "", "")) +
  guides(size=FALSE, shape=FALSE) +
  ## scale_color_manual(values = c("white", "lightblue2"))
  ## stat_summary(fun.data="mean_sdl"
  ##            , fun.args = list(mult=2)
  ##              , geom="pointrange"
  ##              , color="black"
  ##              , size=0.65) +
  ggtitle("RQ3: Statistical significance comparison matrix") +
  ylab("Algorithm") +
  theme(panel.grid.major.y = element_line(color = "lightgrey", linetype="dashed"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom")


## ggsave("../plots/RQ3_PVal.png", plot = pval.plt, height = 4, width = 7, device = "png")
