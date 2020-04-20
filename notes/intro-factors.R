x <- c("red", "gold", "green")
str(x)

# factors is a statistacl variable that is stored as different types known as factor
f <- factor(x)
str(f)

library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/donkeys.csv")

ggplot(df) +
    geom_jitter(aes(Age, Girth), width=0.2)

# Type coercion factor(Age)
# could, but should not
factor(df$Weight)