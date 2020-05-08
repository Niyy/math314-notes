library(ggplot2)
library(dplyr)
library(boot)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/finches.csv")

ggplot(df, aes(island, beakwidth)) +
  geom_jitter() +
  geom_violin(fill=NA)

# Offset from islands
df %>%
  group_by(island) %>%
  summarise(m=mean(beakwidth))

fit <- lm(beakwidth ~ island, data = df)
beta <- coef(fit)

## Will get groupmeans
beta[1] + beta[2]
beta[1] + beta[3]

##hat(beakwidth) = beta[1] + beta[2]*sancristobal + beta[3]*santacruz

##^ most important thing from this lecture: Figuring out how to use catagorical variables into linear model
## by turning them into 1's and 0's.

x <- model.matrix(fit)

head(x)
tail(x)

boot_means <- function(data, idx)
{
  fit <- lm(beakwidth ~ island, data = df[idx,])
  coef(fit)
}

b <- boot(df, boot_means, 1001)
boot.ci(b, index = 1, type = "perc")

boot.ci(b, index = 2, type = "perc")

## We are 95% confidence that the true population mean difference in beakwidth for birds
## on the island san cristobal relative to the island floreana is between 0.17 and 1.9 cm.

boot.ci(b, index = 3, type = "perc")

## We are 95% confidence that the true population mean difference in beakwidth for birds
## on the island santa cruz relative to the island floreana is between -0.2 and 1.5 cm.