library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)
library(Hmisc)

finResultsFile <- "../data/fin_data.csv"
autoResultsFile <- "../data/auto_data.csv"
finRawFile <- "../data/fin_raw_singletons.csv"
autoRawFile <- "../data/auto_raw_singletons.csv"
finCountsFile <- "../data/fin_counts.csv"
autoCountsFile <- "../data/auto_counts.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))
autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

finDF <- finData %>% mutate(data = "Financial") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)
autoDF <- autoData %>% mutate(data = "Auto") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

finCountData  <- read.csv(file=finCountsFile) %>% mutate(data = "Financial")
autoCountData <- read.csv(file=autoCountsFile) %>% mutate(data = "Auto")
countData     <- rbind(finCountData, autoCountData) %>% mutate(SatRatio = Sat / UnSat) %>% mutate(Sat = as.factor(Sat), UnSat = as.factor(UnSat))

data <- rbind(finDF, autoDF)

rq1DF <- data %>% filter(Variants > 2) %>% group_by(Algorithm) %>% arrange(Variants) %>% merge(countData)

rq1 <- ggplot(rq1DF) +
  geom_line(aes(x=Variants, y=Mean, color=Algorithm)) +
  geom_point(aes(x=Variants, y=Mean, shape=Algorithm, color=Algorithm),size=3) +
  scale_shape_manual(values = c(1,2,5,17)) +
  facet_wrap(. ~ data, scales = "free") +
  geom_bar(aes(x=Variants, fill=Sat*100,size=5)) +
  theme_classic() +
  ggtitle("RQ1: Performance as variants increase") +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = "bottom")

## ggsave("../plots/RQ1.png", plot = rq1, height = 4, width = 7, device = "png")

################# Singleton Analysis ##############################

rq3DF <- data %>% filter(Variants <= 2) %>%
  mutate(plotOrdering = as.numeric(substring(Config, 2))) %>%
  mutate(Config = factor(Config, levels = c("V1", "V2", "V3", "V4", "V5", "V6",
                                            "V7", "V8", "V9", "V10")))

rq3 <- ggplot(rq3DF, aes(x=Config, y=Mean, fill=Algorithm, shape=Algorithm, color=Algorithm)) +
  geom_point(size=6) +
  scale_shape_manual(values = c(1,2,5,17)) +
  theme_classic() +
  facet_wrap(.~ data, scales="free") +
  ## stat_summary(fun.data="mean_sdl"
  ##            , fun.args = list(mult=2)
  ##              , geom="pointrange"
  ##              , color="black"
  ##              , size=0.65) +
  ggtitle("RQ3: Overhead of Variational Solving on Plain Formulas") +
  ylab("Time [s] to solve single version variant") +
  xlab("Feature Model Version") +
  theme(legend.position = "right") +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  coord_flip()

ggsave("../plots/RQ3.png", plot = rq3, height = 4, width = 7, device = "png")

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
         data = "Financial") %>%
  select(-Dump) %>%
  ## remove out-group comparisons between different variants, e.g., V1 - V10
  filter(ConfigLeft == ConfigRight) %>%
  ## sort by p-value
  arrange(adj.p.value)

### Check leverage of the observations for outliers
res.fin.outliers <- plot(res.fin.aov, 1)
### observations 251, 148 and 307 are severe outliers for this data


### Check the normality assumption of the ANOVA
res.fin.ass <- plot(res.fin.aov, 2)
### its bad enough to check the stats directly with the shapiro-wilk test

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
         data = "Auto") %>%
  select(-Dump) %>%
  ## remove out-group comparisons between different variants, e.g., V1 - V10
  filter(ConfigLeft == ConfigRight) %>%
  ## sort by p-value
  arrange(adj.p.value)


### Check leverage of the observations for outliers
res.auto.outliers <- plot(res.auto.aov, 1)


### Check the normality assumption of the ANOVA
res.auto.ass <- plot(res.auto.aov, 2)

aov.auto.resids <- residuals(object=res.auto.aov)

## Shapiro-Wilk normality test
res.auto.shaps <- shapiro.test(x = aov.auto.resids)


########################## Plot p-values ##################
options(scipen = 999)
rq3pvDF <- rbind(auto.tuk_res, fin.tuk_res) %>% arrange(adj.p.value)

rq3pv <- ggplot(rq3pvDF, aes(x=AlgLeft, y=AlgRight, shape=data)) +
  geom_point(aes(size=sizeVal)) +
  geom_jitter() +
  theme_classic()
