library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(gridExtra)
require(data.table)
# for mosaic plots
library(ggmosaic)

# Change this to the appropriate directory if you're going to run this script
## setwd("/home/doyougnu/Research/XOP/XOP_Encoding/R/BellackTransTypology")

############################### Data Munging ###################################
                                        # read in each data table

## variables for each file, this is hard coded to correspond to criterion
## output, check app/main.hs
timingsResultsFile <- "../results.csv"
descriptorResults <- "../timings.csv"

## read in timings table
dfTimings <- read.csv(file=timingsResultsFile)

## grab the naming column
nmCol <- dfTimings$Name

## mutate the numerics from characters to numerics
timings <- dfTimings[,2:7] %>% apply(2, as.numeric) %>% as.data.frame

## Reconstruct the data frame
timings$Name <- nmCol
timings <- timings %>% filter(!is.na(Mean))

## Read in descriptor table
dfDesc <- read.csv(file=descriptorResults)

## criterion adds a header for every test, this means we must drop every even
## row because the first header is counted as a head in fread, which means our
## data begins on row 1

## define a sequence of odds (the rows we want to keep)
toDelete <- seq(1, length(dfDesc), 2)

## subset the table using the sequence and convert to numeric
dfDesc <- dfDesc[toDelete,] %>% apply(2, as.numeric) %>% as.data.frame
