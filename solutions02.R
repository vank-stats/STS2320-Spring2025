# STS 2320 - Activity 01

# Read in data
# Put the data in the same file as your script 
# Then set working directory to source file location

library(readxl)

flights <- read_xlsx("kayak2025.xlsx")


# Question 1 - Linear regression predicting cost with distance

flights_slr <- lm(Cost ~ Distance, data = flights)

summary(flights_slr)
cor(flights$Cost, flights$Distance)


# Questions 2 through 5 - No additional code needed


# Question 6 - Using equation for Dallas


358.26247 + 1056 * 0.12705


# Question 7 - Using equation for Sydney


358.26247 + 9637 * 0.12705


# Questions 8 and 9 - No additional code needed


# Question 10 - Using predict() function for Dallas & Sydney

predict(flights_slr, newdata = data.frame(Distance = 1056))
predict(flights_slr, newdata = data.frame(Distance = 9637))


# Question 11 - Calculating residuals

489 - predict(flights_slr, newdata = data.frame(Distance = 1056))
1556 - predict(flights_slr, newdata = data.frame(Distance = 9637))


# Questions 12 and 13 - No additional code needed
