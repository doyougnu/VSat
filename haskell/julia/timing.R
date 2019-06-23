library(ggplot2)
library(dplyr)

timingsResultsFile <- "../data/findata.csv"

data <- read.csv(file=timingsResultsFile)

df <- data %>% filter(CompressionRatio > 0) %>% filter( Config != "V1"
                                                      ,Config != "V2"
                                                      ,Config != "V3"
                                                      ,Config != "V4"
                                                      ,Config != "V5"
                                                      ,Config != "V6"
                                                      ,Config != "V7"
                                                      ,Config != "V8"
                                                      ,Config != "V9"
                                                      ,Config != "V10"
                                                      ,Config != "EvolutionAware")

ggplot(df, mapping = aes(x=(CompressionRatio * 100), y=Mean, shape=Algorithm, fill=Algorithm)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_log10() +
  scale_x_log10() +
  geom_point() + geom_smooth(method = lm, se = FALSE)
  ## geom_col(position = "dodge")
## +
##   facet_grid(Config ~ .)

## ggsave("../plots/auto_cascade",device = "png")
