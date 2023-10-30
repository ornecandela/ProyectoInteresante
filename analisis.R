library(tidyverse)
ggplot(mtcars) +
  aes(x = mpg) +
  geom_histogram()
