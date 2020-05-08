# goals
# *multiple linear regression is the backbone of modern statistics
# *incorporate (almost) any number of explanatory variables to explain one numerical response
# *unique intercepts is the easest example of one categorical and one numerical explanatory variable
# *picture to motivat the idea
# *example in R.

library(ggplot2)
library(dplyr)
library(boot)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/finches.csv")

ggplot(df, aes(middletoelength, beakwidth)) +
  geom_point() +
  geom_smooth(method="lm", formula = "y ~ x", se=FALSE)

fit_ints <- lm(beakwidth ~ island + middletoelength, data = df)
beta_ints <- coef(fit_ints)

X <- model.matrix(fit_ints)
head(x)

sum(beta_ints * X[1, ])
sum(beta_ints * c(1, 0, 0, 20))

df$yhat <- apply(X, 1, function(row) sum(beta_ints * row))


ggplot(df, aes(middletoelength, beakwidth, color = island)) +
  geom_point() +
  geom_line(aes(y=yhat))
# Each line in the above graph has the same slope, but different intercepts

boot_ints <- function(data, idx)
{
  fit_ints <- lm(beakwidth ~ island + middletoelength, data = df[idx, ])
  coef(fit_ints)
}

b <- boot(df, boot_ints, 1001)
boot.ci(b, index = 2, type = "perc")

## We are 95% confident that for any middle toe length, a san cristobal finch's beakwidth is 
## between 0.31 and 1.43 cm larger than the beak width of finches from the island floreana.

boot.ci(b, index = 4, type = "perc")

