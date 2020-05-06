library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)
library(Hmisc)

finResultsFile <- "../data/fin_data.csv"
autoResultsFile <- "../data/auto_data.csv"
finRawFile <- "../data/fin_raw_singletons.csv"

finData <- read.csv(file=finResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

autoData <- read.csv(file=autoResultsFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm))

finDF <- finData %>% mutate(data = "Financial") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)
autoDF <- autoData %>% mutate(data = "Auto") # %>% select(Mean, Algorithm, CompressionRatio, data, ChcCount, PlainCount, Config)

data <- rbind(finDF, autoDF)

rq1DF <- data %>% filter(Variants > 2) %>% group_by(Algorithm) %>% arrange(Variants)

rq1 <- ggplot(rq1DF, mapping = aes(x=Variants, y=Mean, shape=Algorithm, color=Algorithm)) +
  geom_point(size=3) +
  geom_line() +
  scale_shape_manual(values = c(1,2,5,17)) +
  facet_wrap(. ~ data, scales = "free") +
  theme_classic() +
  ggtitle("RQ1: Performance as variants increase") +
  ylab("Time [Min.] to solve all Variants") +
  theme(legend.position = "bottom")

## ggsave("../plots/RQ1.png", plot = rq1, height = 4, width = 7, device = "png")

################# Singleton Analysis ##############################

finSingData <- read.csv(file=finRawFile) %>% mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>% mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Financial")

rq3DF <- finSingData %>%
  group_by(Algorithm, Config) %>%
  mutate(TimeCalc = time -append(0,head(time, -1))) %>%
  filter(TimeCalc > 0) ## remove the filter after running on machine with proper
                       ## memory

rq3 <- ggplot(rq3DF, aes(x=Algorithm, y=TimeCalc, shape=Algorithm, color=Algorithm)) +
  geom_violin(trim=FALSE) +
  geom_jitter() +
  theme_classic() +
  scale_shape_manual(values = c(1,2,5,17)) +
  stat_summary(fun.data="mean_sdl"
             , fun.args = list(mult=2)
               , geom="pointrange"
               , color="black"
               , size=0.65) +
  ggtitle("RQ3: Overhead of Variational Solving on Plain Formulas") +
  ylab("Time [s]") +
  theme(legend.position = "none")


ggsave("../plots/RQ3.png", plot = rq3, height = 4, width = 7, device = "png")

### Do a two-way anova looking at both algorithm and config and their interaction
res.aov <- aov(TimeCalc ~ Algorithm * Config, data = rq3DF)

### Check the summary to see what is significant, all of it is as expected
sig_res <- summary(res.aov)

## Df Sum Sq Mean Sq F value   Pr(>F)
## Algorithm          3  10.28   3.428   9.516 4.71e-06 ***
## Config             9  44.95   4.995  13.866  < 2e-16 ***
## Algorithm:Config  27  16.64   0.616   1.711   0.0167 *
##                                                 Residuals        344 123.91   0.360
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


### Finally, perform the pair-wise Tukey comparison to test the difference
### between groups
tuk_res <- TukeyHSD(res.aov, which = "Algorithm")

## Note that only v-->p and p-->p is not significant

## Tukey multiple comparisons of means
## 95% family-wise confidence level

## Fit: aov(formula = TimeCalc ~ Algorithm * Config, data = rq3DF)

## $Algorithm
##                diff         lwr         upr     p adj
## p⟶v-p⟶p -0.24610722 -0.47102936 -0.02118509 0.0256691
## v⟶p-p⟶p -0.02496165 -0.25159688  0.20167358 0.9919734
## v⟶v-p⟶p  0.21014178 -0.01422963  0.43451319 0.0756077
## v⟶p-p⟶v  0.22114557 -0.00193726  0.44422841 0.0530032
## v⟶v-p⟶v  0.45624900  0.23546641  0.67703159 0.0000010
## v⟶v-v⟶p  0.23510343  0.01257587  0.45763099 0.0337644

### Check leverage of the observations for outliers
res.outliers <- plot(res.aov, 1)
### observations 251, 148 and 307 are severe outliers for this data


### Check the normality assumption of the ANOVA
res.ass <- plot(res.aov, 2)
### its bad enough to check the stats directly with the shapiro-wilk test

aov_resids <- residuals(object=res.aov)
res.shaps <- shapiro.test(x = aov_resids)

## Shapiro-Wilk normality test

## data:  aov_resids
## W = 0.87101, p-value < 2.2e-16

### those outliers are too significant!!!
