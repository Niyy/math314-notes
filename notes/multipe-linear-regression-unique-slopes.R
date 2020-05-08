#Multiple Linear Regression, Unique Slopes
# * multiple linear regression is the backbone of modern statistics
# * incorporate multiple explanetory bariables, such that the number of estimated parmaters is less than
# number of estimated parmaters to less than the sample size.
# * unique slopes; one slope (across a numerical explanatory variable) for each leve of a categorical
#explanetory bariable

library(ggplot2)
library(dplyr)
library(boot)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/finches.csv")

ggplot(df, aes(middletoelength, beakwidth)) +
  geom_point() +
  geom_smooth(method="lm", formula = "y ~ x", se=FALSE)

fit_slps <- lm(beakwidth ~ island:middletoelength, data = df)
# : interacting island with middletoelength
beta_slps <- coef(fit_slps)
## hat(beakwidth) = beta[1] + beta[2]*floreana * middletoelength +
##  beta[3] * sancristobal + middletoelength +
##  beta[4] + santacruz * middletoelength

sum(beta_slps * c(1, 1*20, 0, 0))
# predict a finch with a beakwidth of 20 on the island floreana

# encoded in model matrix
X <- model.matrix(fit_slps)
head(X)
tail(X)

# If yhat does not exist it will make it so
df$yhat <- apply(X, 1, function(row) sum(beta_slps * row))

ggplot(df, aes(middletoelength, beakwidth, color=island)) +
  geom_point() +
  geom_line(aes(y = yhat))

# It might be hard to see, but they do have the same intercept.

boot_slps <- function(data, idx)
{
  fit_slps <- lm(beakwidth ~ island:middletoelength, data = df[idx, ])
  coef(fit_slps)
}

b <- boot(df, boot_slps, 1001)
boot.ci(b, index = 2, type = "perc")

# We are 95% confident that for every 1 cm increase in middletoelength, we expet
# the beak width of finches on the island Floreana to increase by between 0.63 and 1.22 cm.
