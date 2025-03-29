# Activity 6 solutions

# Load required packages

library(readxl) # used to read in data
library(ggplot2) # used for scatter plot


# Read in SAT data
# Note: My data is in a folder, you may need to change the file path for you

sat <- read_excel("Data/SAT.xlsx",
                      sheet = 1)


# Question 2 - Scatterplot

ggplot(sat, aes(x = Salary, y = SAT)) +
  geom_point()


# Question 3 - SLR model

sat_slr <- lm(SAT ~ Salary, data = sat)
summary(sat_slr)


# Question 5 - MLR model

sat_mlr <- lm(SAT ~ Salary + Percent, data = sat)
summary(sat_mlr)


# Question 9 - MLR model with categorical variable

sat_mlr2 <- lm(SAT ~ Salary + PercentCat, data = sat)
summary(sat_mlr2)
