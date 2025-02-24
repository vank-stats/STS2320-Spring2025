# STS 2320 - Activity 04

# Read in data
# Put the data in the same file as your script 
# Then set working directory to source file location

library(readxl)

mammals <- read_excel("mammals.xlsx",
                      sheet = 1)


# Question 1 - Scatterplot

library(ggplot2)
ggplot(mammals, aes(x = total_sleep, 
                    y = life_span)) +
  geom_point()


# Question 2 - Correlation with hypothesis test

cor.test(mammals$total_sleep, 
         mammals$life_span)


# Question 3 - SLR Equation

mammal_slr <- lm(life_span ~ total_sleep, 
                 data = mammals)
mammal_slr


# Question 4 & 5 - No additional code


# Question 6 - Hypothesis test for beta_1

summary(mammal_slr)


# Question 7 - No additional code


# Question 8 - Prediction for dogs (12 hours total sleep) - by hand and using R

36.9326 - 1.6403 * 12

predict(mammal_slr, 
        newdata = data.frame(total_sleep = 12))


# Question 9 - No additional code


# Question 10 - Prediction interval for dogs

predict(mammal_slr, 
        newdata = data.frame(total_sleep = 12),
        interval = "prediction", 
        level = 0.95)


# Question 11 - Confidence interval for species with 12 hours of sleep

predict(mammal_slr, 
        newdata = data.frame(total_sleep = 12),
        interval = "confidence", 
        level = 0.95)


# Question2 12 & 13 - No additional code