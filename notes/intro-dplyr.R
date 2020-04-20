df <- data.frame(x=2:11, y=22:31, f=gl(10, 2))
str(df)

head(df)

install.packages("tidyverse")
library(dplyr)

df %>% summarise(m=mean(x))
mean(df$x)

df %>% filter(f == "2")
df %>% filter(f == "4")


# or chain them together like so

df %>%
  filter(f%in% c("2", "4", "5")) %>%
  group_by(f) %>%
  summarise(m=mean(x))

# As Exercise, try to recreate the above code without dplry

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/master/bike.csv")

df %>%
  group_by(season) %>%
  summarise(m_rented_bikes = mean(cnt), sd_rented_bikes = sd(cnt)) %>%
  arrange(desc(m_rented_bikes))
 