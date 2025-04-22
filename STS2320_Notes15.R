# STS 2320 - Notes 15 Code

# First set your working directory so the file path works
# Load appropriate packages

library(readxl) # to read in Microsoft Excel file


# Read jobs data into R
# My data was in a Data folder in my working directory. Yours may not be.

sleep <- read_excel("Data/sleep.xlsx")


# Create binary 0/1 version of response variable

sleep$seven_yes <- sleep$SevenHrs == "Yes"


# Run logistic regression model predicting probability of seven hours with age

sleep_log <- glm(seven_yes ~ age, family = "binomial", data = sleep)
summary(sleep_log)


# Confidence interval for the age slope

confint(sleep_log)


# Confidence interval for odds ratio of age slope

exp(confint(sleep_log))
