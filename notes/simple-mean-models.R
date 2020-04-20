library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/bike.csv")

# estimates the population distro
ggplot(df, aes(cnt)) + geom_histogram(bins=15)

mean(df$cnt)

ll_normal <- function(mu, x) {
  d <- mu - x
  sum(d * d)
}

optim(5000, ll_normal, method="L-BFGS-B", x=df$cnt)$par

# Built in functions in R that help through this class, I would continue to use optim

# estiamte the sampling distribution of the sample mean
N <- length(df$cnt)
R <- 1001
means <- rep(NA, R)

for(r in 1:R) {
  idx <- sample(N, N, replace=TRUE)
  means[r] <- mean(df$cnt[idx])
}


# remeber two distributions floating around
# sampling distribution
# CI: calculated from (data) sampling distributions
# to guess about the population mean
dfm <- data.frame(x = means)
ggplot(dfm, aes(x)) + geom_density()


dfci <- data.frame(ci = quantile(means, c(0.05, 0.95)))

ggplot() + geom_density(data=dfm, aes(x)) + geom_rug(data=dfci, aes(ci), color="blue")


ggplot() + geom_histogram(data=df, aes(cnt), bins=15) + geom_rug(data=dfci, aes(ci), color="blue")

