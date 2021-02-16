library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(latex2exp)
library(Hmisc)
library(broom)
library(ggpubr)
library(scales)
library(rstatix)
library(car) # for Anova function to avoid base anova function which doesn't
             # considered unbalanced experimental designs. Our design isn't
             # unbalanced but still

finRawFile  <- "../munged_data/financial_raw.csv"
autoRawFile <- "../munged_data/auto_raw.csv"

## use scientific notation
options(scipen = 999)
################# Singleton Analysis ##############################
finRawDF <- read.csv(file=finRawFile) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Financial") %>%
  mutate(Algorithm = as.factor(Algorithm),
         Config = as.factor(Config),
         DataSet = as.factor(DataSet)) %>%
  group_by(Algorithm, DataSet, Config) %>%
  mutate(TimeCalc = Time - lag(Time, default = 0))

autoRawDF <- read.csv(file=autoRawFile) %>%
  mutate(Algorithm = gsub("-->", "\U27f6", Algorithm), data = "Auto") %>%
  mutate(Algorithm = as.factor(Algorithm),
         Config = as.factor(Config),
         DataSet = as.factor(DataSet)) %>%
  group_by(Algorithm, DataSet, Config) %>%
  mutate(TimeCalc = Time - lag(Time, default = 0))

##################### Financial #############################
## Algorithms are significant
fin.alg.res <- kruskal.test(TimeCalc ~ Algorithm, finRawDF)

## Versions are significant as expected
fin.vers.res <- kruskal.test(TimeCalc ~ Config, finRawDF)

## Solvers are actually not significant by themselves
fin.slvr.res <- kruskal.test(TimeCalc ~ DataSet, finRawDF)

## Interaction bewtween algorithm and version significant as expected
fin.alg.conf.inters <- interaction(finRawDF$Algorithm, finRawDF$Config)
fin.alg.slvr.inters <- interaction(finRawDF$DataSet, finRawDF$Algorithm, finRawDF$Config)
fin.alg.conf.res    <- kruskal.test(TimeCalc ~ fin.alg.conf.inters, finRawDF)
fin.alg.slvr.res    <- kruskal.test(TimeCalc ~ fin.alg.slvr.inters, finRawDF)

## Find the pairs which are significant
pairs <- function(time, factor){
  pairwise.wilcox.test(time, factor,
                       p.adj="bonf", exact=FALSE, method="holm",
                       paired = FALSE) %>% tidy %>% arrange(p.value)
}

## find the pairs of algorithms which are signficantly different. You'll notice
## that v-->v is different from each other but the other algorithms are not
## different
fin.pairs <- pairs(finRawDF$TimeCalc, finRawDF$Algorithm)

####################### Fin comparison considering versions ##################
fin.algs.conf.pairs <- pairs(finRawDF$TimeCalc, fin.alg.conf.inters) %>%
  separate(group1, sep=c(3,4), into = c("AlgLeft", "Dump", "ConfigLeft")) %>%
  separate(group2, sep=c(3,4), into = c("AlgRight", "Dump2", "ConfigRight")) %>%
  select(-Dump, -Dump2) %>%
  filter(ConfigRight == ConfigLeft) %>%
  mutate(data = "Financial") %>%
  arrange(p.value)


##################### Auto #############################
## Algorithms are significant
auto.alg.res <- kruskal.test(TimeCalc ~ Algorithm, autoRawDF)

## Versions are significant as expected
auto.vers.res <- kruskal.test(TimeCalc ~ Config, autoRawDF)

## Solvers are actually not significant by themselves
auto.slvr.res <- kruskal.test(TimeCalc ~ DataSet, autoRawDF)

## Interaction bewtween algorithm and version significant as expected
auto.alg.conf.inters <- interaction(autoRawDF$Algorithm, autoRawDF$Config)
## we shouldn't expect the three-way interaction to be significant and this is
## what we find
auto.alg.slvr.inters <- interaction(autoRawDF$DataSet, autoRawDF$Algorithm, autoRawDF$Config)
auto.alg.conf.res    <- kruskal.test(TimeCalc ~ auto.alg.conf.inters, autoRawDF)
auto.alg.slvr.res    <- kruskal.test(TimeCalc ~ auto.alg.slvr.inters, autoRawDF)

## Auto find the pairs which are significant. This is the global case considering all solvers
## notice we hard code and assume the time calc column

## global comparison of algorithms with data from solvers considered
auto.pairs <- pairs(autoRawDF$TimeCalc, autoRawDF$Algorithm)

####################### Auto comparison considering versions ##################
## We know what is significantly different from the kruskal test but we don't
## know exactly what is different so we perform a pairwise wilcox test to
## observe exactly which pairs are different. We notice here that the p-values
## are all 1 after the bonferroni adjustment. Solver is not statistically
## significant for both datasets
auto.algs.conf.pairs <- pairs(autoRawDF$TimeCalc, auto.alg.conf.inters) %>%
  separate(group1, sep="\\.", into = c("AlgLeft", "ConfigLeft")) %>%
  separate(group2, sep="\\.", into = c("AlgRight", "ConfigRight")) %>%
  filter(ConfigRight == ConfigLeft) %>%
  mutate(data = "Auto") %>%
  arrange(p.value)

############################# Combined data frame ##################
rq3pvDF <- rbind(auto.algs.conf.pairs, fin.algs.conf.pairs) %>%
  arrange(p.value) %>%
  mutate(Significance = case_when(p.value <= 0.05 ~ "Significant",
                                  TRUE ~ "Not Significant"),
         Version = factor(ConfigLeft, levels =
                                        c("V1", "V2", "V3", "V4", "V5", "V6",
                                          "V7", "V8", "V9", "V10")))

###################### RQ2 speedup significance ###############################

### average by solver speedup
speedupByVariantSolver <- function(df) {
  df %>%
    select(DataSet, Algorithm, Variants, Mean) %>%
    group_by(DataSet,Algorithm,Variants) %>%
    summarise(Mean = mean(Mean)) %>%
    mutate(Speedup = lag(Mean, default = first(Mean)) / Mean)
}

speedupBySolver <- function(df) {
  df %>%
    select(DataSet, Algorithm, Variants, Mean) %>%
    group_by(DataSet,Algorithm) %>%
    summarise(Mean = mean(Mean)) %>%
    mutate(Speedup = lag(Mean, default = first(Mean)) / Mean)
}

rq1AutoSpeedupBySolver <- speedupBySolver(rq1DFAuto)
rq1FinSpeedupBySolver  <- speedupBySolver(rq1DFFin)

### average speedup
avgSpeedupFin <- rq1AutoSpeedupBySolver %>%
  group_by(Algorithm) %>%
  summarise(Speedup = mean(Speedup))

avgSpeedupAuto <- rq1FinSpeedupBySolver %>%
  group_by(Algorithm) %>%
  summarise(Speedup = mean(Speedup))

######################### Speedup Significance test ###########################
## we know that v-->v is significantly different that the other algorithms but
## how much of the performance can we attribute to base solver? we fit a
## genearlized linear model, glm, to the data to assess the contribution of the
## base solver

### Perform the anova
lmDFAuto <- autoRawDF %>% select(TimeCalc,DataSet,Algorithm,Config)
## lmDFAuto$DataSet <- factor(lmDFAuto$DataSet, levels = c("Z3", "Boolector", "CVC4","Yices"))
lmDFAuto <- within(lmDFAuto, DataSet <- relevel(DataSet, ref="Z3"))
lmDFAuto <- within(lmDFAuto, Algorithm <- relevel(Algorithm, ref="v\U27f6v"))
model.auto <- lm(TimeCalc ~ DataSet + Algorithm + Config, lmDFAuto)

### now the ANOVA

## Anova Table (Type II tests)
##
## Response: TimeCalc
## Sum Sq  Df  F value                Pr(>F)
## DataSet    1819868   3   8.9666             0.0000149 ***
## Algorithm 15167995   3  74.7338 < 0.00000000000000022 ***
## Config    69549059   3 342.6734 < 0.00000000000000022 ***
## Residuals 11704038 173
## ---
  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## we see that dataset does explain some variation! Let's get estimates

## > summary(model)
##
## Call:
## lm(formula = TimeCalc ~ DataSet + Algorithm + Config, data = lmDFAuto)
##
## Residuals:
##     Min      1Q  Median      3Q     Max
## -787.13 -107.14  -12.32  105.85  521.34
##
## Coefficients:
##                   Estimate Std. Error t value             Pr(>|t|)
## (Intercept)        -297.00      60.20  -4.933           0.00000189 ***
## DataSetBoolector   -113.15      54.10  -2.092              0.03793 *
## DataSetCVC4         102.04      54.10   1.886              0.06094 .
## DataSetYices       -155.02      54.10  -2.866              0.00468 **
## Algorithmp⟶p        669.41      53.09  12.608 < 0.0000000000000002 ***
## Algorithmp⟶v        577.05      53.09  10.869 < 0.0000000000000002 ***
## Algorithmv⟶p        707.84      56.76  12.471 < 0.0000000000000002 ***
## ConfigV1*V2         309.42      56.76   5.452           0.00000017 ***
## ConfigV1*V2*V3      754.97      56.76  13.301 < 0.0000000000000002 ***
## ConfigV1*V2*V3*V4  1655.92      56.76  29.175 < 0.0000000000000002 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 260.1 on 173 degrees of freedom
## Multiple R-squared:  0.8851,	Adjusted R-squared:  0.8791
## F-statistic: 148.1 on 9 and 173 DF,  p-value: < 0.00000000000000022

## For auto we find that only CVC4, relative to Z3 does not correlate to a
## runtime increase, however Boolector and Yices do correlate to a speedup
## relative to Z3

lmDFFin <- finRawDF %>% select(TimeCalc,DataSet,Algorithm,Config)
## lmDFFin$DataSet <- factor(lmDFFin$DataSet, levels = c("Z3", "Boolector", "CVC4","Yices"))
lmDFFin <- within(lmDFFin, DataSet <- relevel(DataSet, ref="Z3"))
lmDFFin <- within(lmDFFin, Algorithm <- relevel(Algorithm, ref="v\U27f6v"))
model.fin <- lm(TimeCalc ~ DataSet + Algorithm + Config, lmDFFin)
