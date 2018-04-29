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

## tidy up the data, each column is a variable and each row is an observation
timings <- timings %>%
  separate(Name, into = c("shared", "scale", "Operation"), sep = "\\/")

## Read in descriptor table
dfDesc <- read.csv(file=descriptorResults)

## Clean up the trailing "_" in the column names
names(dfDesc) <- gsub(pattern = "_", "", x = names(dfDesc))

## criterion adds a header for every test, this means we must drop every even
## row because the first header is counted as a head in fread, which means our
## data begins on row 1

## define a sequence of evens (the rows we want to drop)
toDelete <- seq(0, length(dfDesc), 2)

## subset the table using the sequence and convert to numeric
dfDesc <- dfDesc[-toDelete,,drop=F]

## save the shared column
shared <- dfDesc$shared

## coerce the numbers to numbers
dfDesc <- dfDesc[,2:7] %>% apply(2, as.numeric) %>% as.data.frame

## add it back to the data frame
dfDesc$shared <- shared

## now merge the tables to a data frame
df <- merge(timings, dfDesc)

############################## Plotting ########################################
plot <- ggplot(df, aes(x=scale, y=Mean, color=Operation)) +
  geom_point() +
  geom_smooth(method=lm, se=T) +
  ylab("Mean [s]")
