# Activity 6 solutions

# Load required packages

library(readxl) # used to read in data
library(broom) # used for augment() function
library(ggplot2) # used for scatter plot
library(dplyr) # used for filter() function to remove outliers


# Read in mammals data
# Note: My data is in a folder, you may need to change the file path for you

mammals <- read_excel("Code and Data/Data/Mammals.xlsx",
                      sheet = 1)


# Question 1 - SLR of total_sleep to predict life_span

mammals_lm <- lm(life_span ~ total_sleep, data = mammals)
summary(mammals_lm)


# Questions 2 - 4 - Outliers, leverage, and influence

mammals_diag <- augment(mammals_lm)
View(mammals_diag)


# Question 5 - Remove outlier and refit

mammals2 <- filter(mammals, life_span < 90)
mammals2_lm <- lm(life_span ~ total_sleep, data = mammals2)
summary(mammals2_lm)


# Question 6 - Outliers, leverage, and influence

mammals2_diag <- augment(mammals2_lm)
View(mammals2_diag)


# Question 7 - Remove outlier and refit

mammals3 <- filter(mammals, life_span < 60)
mammals3_lm <- lm(life_span ~ total_sleep, data = mammals3)
summary(mammals3_lm)


# Question 7 - Bonus graph

ggplot(mammals, aes(x = total_sleep, y = life_span)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 33.8162, slope = -1.4798, linetype = 2) +
  geom_abline(intercept = 30.4483, slope = -1.2360, linetype = 3) +
  theme_bw() +
  labs(title = "Predicting life_span with total_sleep",
       subtitle = "Dashed line removes biggest outlier,\ndotted line removes next biggest also")


# Question 8

brain_lm <- lm(life_span ~ brain_wt, data = mammals)
summary(brain_lm)

brain_diag <- augment(brain_lm)
View(brain_diag)


# Question 9

mammals4 <- filter(mammals, brain_wt < 5000)
brain2_lm <- lm(life_span ~ brain_wt, data = mammals4)
summary(brain2_lm)


# Question 10

brain2_diag <- augment(brain2_lm)
View(brain2_diag)

mammals5 <- filter(mammals, brain_wt < 4000)
brain3_lm <- lm(life_span ~ brain_wt, data = mammals5)
summary(brain3_lm)
