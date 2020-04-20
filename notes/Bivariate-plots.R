library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/donkeys.csv")

 # Bivariate plots of population distrobution
ggplot(df, aes(Sex, Height)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(fill=NA)
# lower and upper wiskers help determine skew (geom_boxplot)
# lower bar is the first quartile
# middle bar is the second quartile 50 percentile
# upper bar is the third quartile 75 percentile
# outliers will be out side the wiskers

ggplot(df, aes(Sex, Height)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(fill=NA, width=0.2) +
  geom_violin(fill=NA, color="blue")
# violin shows vertical density graphs

## bivariate plots of the sampling distribution
install.packages("Hmisc")

ggplot(df, aes(Sex, Girth)) +
  stat_summary(fun.data = "mean_cl_boot")

# By default the line is the bootstrapped
