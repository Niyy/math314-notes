# Multiple Linear Regression, unique intercepts and slopes
# * Multiple Linear regression is the backbone of modern statistics
# * incorporate multiple explanatory variables, such that the number of estimated parmaters
#is less than the sample size.
# * For now only one categorical and one numerical explanatory variable, for simplicity.
# * with this understanding, could explore multiple categorical and numerical explanetory
#vars
# * picture to motivate the idea
# * example in R
library(ggplot2)
library(dplyr)
library(boot)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/finches.csv")

ggplot(df, aes(middletoelength, beakwidth, color = island)) +
  geom_point() +
  geom_smooth(method="lm", formula = "y ~ x", se=FALSE)

fit <- lm(beakwidth ~ island * middletoelength, data = df)
beta <- coef(fit)

## hat(beakwidth) = beta[1] + beta[2] * sancristobal + beta[3] * santacruz
##  beta[4] * middletoelength + beta[5] * sancristobal * middletoelength +
##  beta[6] * santacruz * middletoelength

## san cristobal with middletoelength of 19
sum(beta * c(1, 1, 0, 19, 1 * 19, 0))

X <- model.matrix(fit)

df$yhat <- apply(X, 1, function(row) sum(beta * row))

ggplot(df, aes(middletoelength, beakwidth, color = island)) +
  geom_point() + 
  geom_line(aes(y = yhat))

boot_mlr <- function(data, idx)
{
  fit <- lm(beakwidth ~ island * middletoelength, data = df[idx, ])
  coef(fit)
}

b <- boot(df, boot_mlr, 1001)
boot.ci(b, type = "perc", index = 3)

# We are 95% confident that when middle toe length is equal to 0, we expect finches from the
# island santa cruz to have a beak width larger than finches from the island floreana by 
# between -7.1 and 13.1 cm.

boot.ci(b, type = "perc", index = 6)

# We are 95% confident that for every 1 cm increase in middle toe length, the increase in
# beak width for finches from the island santa cruz is greater than the increase in beak
# width for finches from the island floreana by between -0.7 and 0.4 cm.

boot.ci(b, type = "perc", index = 5)

boot_mlr_predictions <- function(data, idx)
{
  fit <- lm(breakwidth ~ island * middletoelength, data = df[idx, ])
  beta <- coef(fit)
  sum(beta * x) # x needs to be a vector of the same length as beta
}

# index is which beta you are using

b <- boot(df, boot_mlr_predictions, 1001, x = c(1, 1, 0, 19, 1 * 19, 0))
boot.ci(b, type = "perc")

# We are 95% confident that when a finch san cristobal has middle toe length of 19cm
# we expect the beak width to between 10 and 11.1 cm.