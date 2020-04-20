install.packages("Hmisc")
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/donkeys.csv")

ggplot(df, aes(Sex, Girth)) +
  stat_summary(fun.data = "mean_cl_boot")

