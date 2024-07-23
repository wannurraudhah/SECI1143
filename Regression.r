# data
library(ggplot2)
library(tidyverse)
library(magrittr)

# structure of data
str(regression)

# scatter plot
plot(regression$streams, regression$in_apple_charts) +geom_point()
regression %>% ggplot(aes(x = streams, y = in_apple_charts)) +geom_point()

#correlation
cor(regression$streams, regression$in_apple_charts)

#fitting linear model
ml = lm(streams ~ in_apple_charts, data = regression)
ml

#getting summary of the model
summary(ml)
