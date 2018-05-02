library(ggplot2)
library(scales)
library(dplyr)
library(svglite)
library(tidyr)
library(gridExtra)
library(plyr)
require(data.table)
# for mosaic plots
library(ggmosaic)

# Change this to the appropriate directory if you're going to run this script
## setwd("/home/doyougnu/Research/XOP/XOP_Encoding/R/BellackTransTypology")

############################### Data Munging ###################################
                                        # read in each data table

## variables for each file, this is hard coded to correspond to criterion
## output, check app/main.hs the timing results are the actual measurements
## taken by criterion and the recorded bgroup names
## timingsResultsFile <- "../data/2018-05-01_timing_results.csv"
timingsResultsFile <- "../timing_results.csv"

## the descriptor results are the hand crafted descriptor functions for each
## measurement that are recorded to a csv via cassava, these are things like
## number of choices in the prop, number of terms etc.
## descriptorsFile <- "../data/2018-05-01_desc_results.csv"
descriptorsFile <- "../desc_results.csv"

## Given a dataframe that assumes the output structure of criterion's --csv call
## clean up the data frame by converting numerics to numerics while maintaining
## the names columns and tidy up the data set
cleanTimings <- function(df_) {

  ## grab the naming column
  nmCol <- df_$Name

  ## mutate the numerics from characters to numerics
  df_ <- df_[,2:(ncol(df_))] %>% apply(2, as.numeric) %>% as.data.frame

  ## Reconstruct the data frame
  df_$Name <- nmCol
  df_ <- df_ %>% filter(!is.na(Mean))

  ## tidy up the data, each column is a variable and each row is an observation
  df_ <- df_ %>%
    separate(Name, into = c("shared", "runNum", "scale", "Operation"), sep = "\\/") %>%
    mutate(  scale = as.numeric(scale)
           , runNum = as.numeric(runNum)
           , shared = as.factor(shared))

  ## return
  df_
}

cleanDesc <- function(df_) {

  ## Clean up the trailing "_" in the column names
  names(df_) <- gsub(pattern = "_", "", x = names(df_))

  ## criterion adds a header for every test, this means we must drop every even
  ## row because the first header is counted as a head in fread, which means our
  ## data begins on row 1

  ## define a sequence of evens (the rows we want to drop)
  toDelete <- seq(2, nrow(df_), 2)

  ## subset the table using the sequence and convert to numeric
  df_ <- df_[-toDelete,,drop=F]
  ## save the shared column
  shared <- df_$shared

  ## coerce the numbers to numbers
  df_ <- df_[,2:(ncol(df_))] %>% apply(2, as.numeric) %>% as.data.frame

  ## add it back to the data frame
  df_$shared <- shared

  ## return
  df_
}

## helper function, given a file name, read in the data table and then apply a
## unary function that works on tables to it
readAndClean <- function(fname, f) { read.csv(file=fname) %>% f }

## Read in the Timings table
timings <- readAndClean(timingsResultsFile, cleanTimings)

## Read in descriptor table
descriptors <- readAndClean(descriptorsFile, cleanDesc)

## now merge the tables to a data frame
df <- join(timings, descriptors)

save <- function(name, plot) {
  ggsave(file = paste("plots/", name, ".pdf", sep="")
       , device = "pdf"
       , dpi = 300
       , limitsize = TRUE
       , scale = 1
       , width = NA
       , height = NA
       , units = c("in", "cm", "mm")
       , plot = plot)}

############################## Plotting ########################################
noPlains <- df %>% filter(numChc > 0)

bfComparison <- ggplot(noPlains, aes(x=scale, y=Mean, color=Operation)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Mean Solution Time [s]") +
  xlab("Term size") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(title= "Comparison of Brute Force Solve vs Other Methods",
       subtitle="Data generated with 5 resamples, and 5 replications")

# save
save("bfcomparison", bfComparison)

## filter out all brute force data
noBF <- noPlains %>% filter(Operation != "Brute Force")

andIncComp <- ggplot(noBF, aes(x=scale, y=Mean, color=Operation)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Mean Solution Time [s]") +
  xlab("Term size") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(title="And Decomposition vs Incremental Solve",
       subtitle="Data generated with 5 resamples, and 5 replications")

save("andIncComparison", andIncComp)

andIncCompByChc <- ggplot(noBF, aes(x=numChc, y=Mean, color=Operation)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab("Mean Solution Time [s]") +
  xlab("Number of Choices") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(title="And Decomposition vs Incremental Solve",
       subtitle="Data generated with 5 resamples, and 5 replications")

save("andIncComparisonByChoice", andIncCompByChc)
