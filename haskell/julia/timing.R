library(ggplot2)

timingsResultsFile <- "data.csv"

data <- read.csv(file=timingsResultsFile)

ggplot(data, aes(x=PlainCount,
                 y=Mean,color=as.factor(ChcCount))) +
  geom_smooth() + geom_point(size=3) +
  facet_grid(VariantSize ~ ChcCount)
