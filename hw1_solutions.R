# Read in pain data
# Remember to put script and data in same folder. Then set working directory
#   to Source File Location (that same folder)

library(readr)
library(ggplot2)

pain <- read_csv("pain.csv")


# Question 1

ggplot(pain, aes(x = Age, y = Estimated_Pain)) + 
  geom_point()


# Question 2

pain_lm <- lm(Estimated_Pain ~ Age, data = pain)
summary(pain_lm)


# Question 3

cor(pain$Estimated_Pain, pain$Age)
mean(pain$Estimated_Pain)
mean(pain$Age)
sd(pain$Estimated_Pain)
sd(pain$Age)


# Questions 4 to 8 - no additional code

# Question 9

anova(pain_lm)


# Questions 10 and 11 - no additional code

# Question 12

confint(pain_lm)
