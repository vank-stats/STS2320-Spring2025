# STS 2320 - Activity 03

# Read in data
# Put the data in the same file as your script 
# Then set working directory to source file location

library(readxl)

baseball <- read_xlsx("mlb_batting_2024.xlsx")


# Question 1 - Scatterplots of OBP and SLG vs. RPG (load ggplot2 library)

library(ggplot2)

ggplot(baseball) +
  geom_point(aes(x = OBP, y = RPG))

ggplot(baseball) +
  geom_point(aes(x = SLG, y = RPG))


# Question 2 - find R-squared and MSE for SLR models

obp_model <- lm(RPG ~ OBP, data = baseball)
summary(obp_model)
anova(obp_model)

slg_model <- lm(RPG ~ SLG, data = baseball)
summary(slg_model)
anova(slg_model)


# Questions 3 through 6 - no additional code needed


# Question 7 - confidence interval for the slope

confint(obp_model)


# Question 8 - no additional code needed


# Question 9 - Find R-squared and MSE for model using OPS as explanatory var.

ops_model <- lm(RPG ~ OPS, data = baseball)
summary(ops_model)
anova(ops_model)


# Question 10 - find critical value for alpha = 0.01

qt(.995, 28)


# Question 11 - find p-value that was rounded in other output

2 * pt(-17.78, 28)
  