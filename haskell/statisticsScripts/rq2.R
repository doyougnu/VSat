library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

finRawFile         <- "../munged_data/financial_raw.csv"
autoRawFile        <- "../munged_data/auto_raw.csv"

################# RQ2 Statistical Analysis ##############################

finRawDF <- read.csv(file=finRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Financial") %>%
  group_by(Algorithm, DataSet, Config) %>%
  mutate(TimeCalc = Time - lag(Time, default = 0))

autoRawDF <- read.csv(file=autoRawFile) %>%
  mutate(Algorithm = as.factor(Algorithm), Config = as.factor(Config)) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Auto") %>%
  group_by(Algorithm, DataSet, Config) %>%
  mutate(TimeCalc = Time - lag(Time, default = 0))

### Do a two-way anova looking at both algorithm and config and their interaction
res.fin.aov <- aov(TimeCalc ~ Algorithm * DataSet * Config, data = finRawDF)

## ### Check the summary to see what is significant, all of it is as expected
res.fin.sig <- summary(res.fin.aov)

### Finally, perform the pair-wise Tukey comparison to test the difference
### between groups
fin.tuk_res <- TukeyHSD(res.fin.aov, which = "Algorithm:DataSet:Config") %>%
  tidy %>%
  ### cleanup
  separate(contrast,
           sep="-|:"
         , into = c("AlgLeft", "SolverLeft", "ConfigLeft", "AlgRight", "SolverRight", "ConfigRight"), remove = FALSE) %>%
  select(-contrast) %>%
  mutate(data = "Financial",
         pVal = scientific(adj.p.value, 3)) %>%
  ## remove out-group comparisons between different variants, e.g., V1 - V10
  filter(ConfigLeft == ConfigRight)

## Now we have to split the data set into solver comparisons and algorithm comparisons
fin.solver.tres <- fin.tuk_res %>% filter(AlgLeft == AlgRight) %>% arrange(adj.p.value)
fin.alg.tres <- fin.tuk_res %>% filter(SolverLeft == SolverRight) %>% arrange(adj.p.value)

## ### Check leverage of the observations for outliers
res.fin.outliers <- plot(res.fin.aov, 1)
## ### observations 251, 148 and 307 are severe outliers for this data


## ### Check the normality assumption of the ANOVA
res.fin.ass <- plot(res.fin.aov, 2)

## ## check residuals
aov.fin.resids <- residuals(object=res.fin.aov)

## ## perform Shapiro-wilk normality test
res.fin.shaps <- shapiro.test(x = aov.fin.resids)

## Shapiro-Wilk normality test
## > res.fin.shaps
## data:  aov.fin.resids
## W = 0.52176, p-value < 2.2e-16

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
