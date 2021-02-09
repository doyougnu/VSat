library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

finResultsFile <- "../munged_data/financial.csv"
autoResultsFile <- "../munged_data/auto.csv"
## finRawFile <- "../data/fin_rq3_singletons.csv"
## autoRawFile <- "../data/auto_rq3_singletons.csv"

finData <- read.csv(file=finResultsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), Mean = Time) %>% select(-Time)

autoData <- read.csv(file=autoResultsFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), Mean = Time) %>% select(-Time)

finDF <- finData %>% mutate(data = "Fin")
autoDF <- autoData %>% mutate(data = "Auto")

data <- rbind(finDF, autoDF)

rq1DF <- data %>% filter(ChcCount != 0) %>% group_by(Algorithm) %>%
  arrange(Variants)

rq1DFAuto <- rq1DF %>% filter(data == "Auto")
rq1DFFin <- rq1DF %>% filter(data == "Fin")

breaksRq1 <- function(x) {
  if (max(x) < 17) {
    2^(1:4)
  } else {
    2^(7:10)}
  }

rq1Auto <- ggplot(rq1DFAuto) +
  geom_line(aes(x=Variants, y=Mean/60, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean/60, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  facet_grid(data ~ DataSet, scales = "free_x") +
  theme_classic() +
  scale_x_continuous(breaks=breaksRq1, limits=c(2,NA)) +
  ggtitle("RQ1: Performance as variants increase per base solver") +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = c(0.08,0.75))

rq1Fin <- ggplot(rq1DFFin) +
  geom_line(aes(x=Variants, y=Mean/60, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean/60, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,6,5,17)) +
  facet_grid(data ~ DataSet,scales="free") +
  theme_classic() +
  scale_x_continuous(breaks=breaksRq1, limits=c(2,NA)) +
  ggtitle("RQ1: Performance as variants increase per base solver") +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = c(0.08,0.75), axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1))

ggsave("../plots/RQ1Auto.png", plot = rq1Auto, height = 4, width = 7, device = "png")
ggsave("../plots/RQ1Fin.png", plot = rq1Fin, height = 4, width = 7, device = "png")

################# Singleton Analysis ##############################
## head(-4) %>%
## Need to filter out version variants which have choices for rq1
## rq3DF <- data %>% filter(ChcCount == 0) %>%
##   mutate(plotOrdering = as.numeric(substring(Config, 2))) %>%
##   mutate(Config = factor(Config, levels = c("V1", "V2", "V3", "V4", "V5", "V6",
##                                             "V7", "V8", "V9", "V10")))

## ## custom breaks for the facets
## breaksRq3 <- function(x) {
##   if (max(x) < 4) {
##     ## then we are in fin, NA to just have R find the max
##     seq(0, 1.2, 0.10)
##   } else {
##     seq(0, 150, 10)
##   }
## }

## rq3 <- ggplot(rq3DF, aes(x=Config, y=Mean, fill=Algorithm, shape=Algorithm, color=Algorithm)) +
##   geom_point(size=6) +
##   scale_shape_manual(values = c(1,6,5,17)) +
##   theme_classic() +
##   scale_y_continuous(limits=c(0, NA), breaks=breaksRq3) +
##   facet_wrap(DataSet ~ data, scales="free") +
  ## stat_summary(fun.data="mean_sdl"
  ##            , fun.args = list(mult=2)
  ##              , geom="pointrange"
  ##              , color="black"
  ##              , size=0.65) +
  ## ggtitle("RQ3: Overhead of Variational Solving on Plain Formulas") +
  ## ylab("Time [s] to solve single version variant") +
  ## xlab("Feature Model Version") +
  ## theme(legend.position = c(0.42,0.75),
  ##       legend.key.size = unit(.65,'cm')) +
  ## theme(panel.grid.major.y = element_line(color = "grey"))
  ## coord_flip()

## ## ggsave("../plots/RQ3.png", plot = rq3, height = 4, width = 7, device = "png")

## slow_down <- rq3DF %>% group_by(data,Algorithm) %>%  summarise(AvgMean = mean(Mean))

## finSingData <- read.csv(file=finRawFile) %>%
##   mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
##   mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Financial") %>%
##   group_by(Algorithm, Config) %>%
##   mutate(TimeCalc = time -append(0,head(time, -1))) %>% filter(TimeCalc > 0)


## autoSingData <- read.csv(file=autoRawFile) %>%
##   mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
##   mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Auto") %>%
##   group_by(Algorithm, Config) %>%
##   mutate(TimeCalc = time -append(0,head(time, -1))) %>% filter(TimeCalc > 0)

## ### Do a two-way anova looking at both algorithm and config and their interaction
## res.fin.aov <- aov(TimeCalc ~ Algorithm * Config, data = finSingData)

## ### Check the summary to see what is significant, all of it is as expected
## res.fin.sig <- summary(res.fin.aov)

## ### Finally, perform the pair-wise Tukey comparison to test the difference
## ### between groups
## fin.tuk_res <- TukeyHSD(res.fin.aov, which = "Algorithm:Config") %>%
##   tidy %>%
##   ### cleanup
##   separate(comparison, sep=c(3, 4, 7, 11)
##          , into = c("AlgLeft", "Dump", "ConfigLeft", "AlgRight", "ConfigRight")) %>%
##   mutate(ConfigLeft = gsub("-", "", ConfigLeft),
##          ConfigRight = gsub(":","", ConfigRight),
##          AlgRight = gsub(":|-", "", AlgRight),
##          data = "Financial",
##          pVal = scientific(adj.p.value, 3)) %>%
##   select(-Dump) %>%
##   ## remove out-group comparisons between different variants, e.g., V1 - V10
##   filter(ConfigLeft == ConfigRight) %>%
##   mutate(Comparison=paste(AlgLeft,":",AlgRight,":",ConfigLeft, sep=""),
##          AlgComparison=paste(AlgLeft,":",AlgRight, sep="")) %>%
##   ## sort by p-value
##   arrange(adj.p.value)

## ### Check leverage of the observations for outliers
## ## res.fin.outliers <- plot(res.fin.aov, 1)
## ### observations 251, 148 and 307 are severe outliers for this data


## ### Check the normality assumption of the ANOVA
## ## res.fin.ass <- plot(res.fin.aov, 2)

## ## check residuals
## aov.fin.resids <- residuals(object=res.fin.aov)

## ## perform Shapiro-wilk normality test
## res.fin.shaps <- shapiro.test(x = aov.fin.resids)

## ## Shapiro-Wilk normality test

## ## data:  aov_resids
## ## W = 0.87101, p-value < 2.2e-16

## ### those outliers are too significant!!!

## ############################# Auto ANOVA ##############################
## ### Do a two-way anova looking at both algorithm and config and their interaction
## res.auto.aov <- aov(TimeCalc ~ Algorithm * Config, data = autoSingData)

## ### Check the summary to see what is significant, all of it is as expected
## res.auto.sig <- summary(res.auto.aov)

## ### Autoally, perform the pair-wise Tukey comparison to test the difference
## ### between groups
## auto.tuk_res <- TukeyHSD(res.auto.aov, which = "Algorithm:Config") %>%
##   tidy %>%
## ### cleanup
##   separate(comparison, sep=c(3, 4, 7, 11)
##          , into = c("AlgLeft", "Dump", "ConfigLeft", "AlgRight", "ConfigRight")) %>%
##   mutate(ConfigLeft = gsub("-", "", ConfigLeft),
##          ConfigRight = gsub(":","", ConfigRight),
##          AlgRight = gsub(":|-", "", AlgRight),
##          data = "Auto",
##          pVal = scientific(adj.p.value, 3),
##          Comparison=paste(AlgLeft,":",AlgRight,":",ConfigLeft, sep=""),
##          AlgComparison=paste(AlgLeft,":",AlgRight, sep="")) %>%
##   select(-Dump) %>%
##   ## remove out-group comparisons between different variants, e.g., V1 - V10
##   filter(ConfigLeft == ConfigRight) %>%
##   ## sort by p-value
##   arrange(adj.p.value)


## ### Check leverage of the observations for outliers
## ## res.auto.outliers <- plot(res.auto.aov, 1)


## ### Check the normality assumption of the ANOVA
## res.auto.ass <- plot(res.auto.aov, 2)

## aov.auto.resids <- residuals(object=res.auto.aov)

## ## Shapiro-Wilk normality test
## res.auto.shaps <- shapiro.test(x = aov.auto.resids)

## ## also fails, we'll do a one-way kruskall walis test
## ## Please see sigTest.R
