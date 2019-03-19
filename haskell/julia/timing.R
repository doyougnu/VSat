library(ggplot2)

timingsResultsFile <- "../data/data.csv"

data <- read.csv(file=timingsResultsFile)

ggplot(data, aes(x=PlainCount,
                 y=Mean,color=FormulaType)) +
  geom_smooth() +  geom_jitter() +
  facet_grid(VariantSize ~ ChcCount)
