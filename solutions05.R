# Load required packages

library(readxl) # used to read in data
library(ggplot2) # used for scatter plot
library(gglm) # used for diagnostic plots
library(dplyr) # used for mutate() function to create variables


# Example 1 - Read in data
# Note: My data is in a folder, you may need to change the file path for you

eggs <- read_excel("Code and Data/Data/eggs.xlsx",
                      sheet = 1)


# Question 1 - Fit SLR and generate scatterplot and diagnostic graphs

eggs_lm <- lm(eggs_price ~ whole_chicken_price, data = eggs)

ggplot(eggs, aes(x = whole_chicken_price, y = eggs_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gglm(eggs_lm)


# Question 3 - Add log_eggs variable

eggs <- mutate(eggs, log_eggs = log(eggs_price))


# Question 4 - Refit model and diagnostics with log_eggs

logeggs_lm <- lm(log_eggs ~ whole_chicken_price, data = eggs)

ggplot(eggs, aes(x = whole_chicken_price, y = log_eggs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gglm(logeggs_lm)


# Question 5 - Predicting egg price

predict(logeggs_lm, newdata = data.frame(whole_chicken_price = 1.49))

exp(0.5932777)



# Example 2 - Car stopping

# Question 6 - Read in data, fit SLR model, generate scatterplot and diagnostics

cars <- read_excel("Code and Data/Data/Stopping.xlsx",
                   sheet = 1)

cars_lm <- lm(distance ~ speed, data = cars)

ggplot(cars, aes(x = speed, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gglm(cars_lm)


# Question 9 - Create log_dist variable, re-fit model, generate graphs

cars <- mutate(cars, log_dist = log(distance))

logdist_lm <- lm(log_dist ~ speed, data = cars)

ggplot(cars, aes(x = speed, y = log_dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gglm(logdist_lm)


# Question 10 - Create log_speed variable, re-fit model, generate graphs

cars <- mutate(cars, log_speed = log(speed))

cars_twologs_lm <- lm(log_dist ~ log_speed, data = cars)

ggplot(cars, aes(x = log_speed, y = log_dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gglm(cars_twologs_lm)


# Question 14 - Create sqrt_dist variable, re-fit model, generate graphs

cars <- mutate(cars, sqrt_dist = sqrt(distance))

sqrtdist_lm <- lm(sqrt_dist ~ speed, data = cars)

ggplot(cars, aes(x = speed, y = sqrt_dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

gglm(sqrtdist_lm)
