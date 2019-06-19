library(ggplot2)

timingsResultsFile <- "../data/data.csv"

data <- read.csv(file=timingsResultsFile)

ggplot(data, mapping = aes(x=PlainCount, y=Mean, shape=Algorithm, color=Algorithm, group=Algorithm)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(trans='log10') + geom_point() + geom_line()
## +
##   facet_grid(Config ~ .)

## ggsave("../plots/auto_cascade",device = "png")
