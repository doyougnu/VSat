library(ggplot2)

timingsResultsFile <- "../data/data.csv"

data <- read.csv(file=timingsResultsFile)

ggplot(data, mapping = aes(x=Config, y=Mean, color=Algorithm, group=Algorithm)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() + geom_line()
## +
##   facet_grid(Config ~ .)

ggsave("../plots/financial_cascade",device = "png")
