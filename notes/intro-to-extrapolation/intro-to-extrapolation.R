library(ggplot2)
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/cars.csv")

ggplot(df, aes(weight, mpgCity)) +
  geom_point() +
  geom_smooth(method="lm", formula="y ~ x", se = FALSE)

fit <- lm(mpgCity ~ weight, data = df)
(beta <- coef(fit))

sum(beta * c(1, 3000))

sum(beta * c(1, 6000))

## Extrapolation