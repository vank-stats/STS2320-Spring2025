# Homework 3 Solutions

# Read in data

pain <- read.csv("pain.csv")


# Question2 1 - 4

pain_lm <- lm(Estimated_Pain ~ Gender, data = pain)
summary(pain_lm)


# Question 5

pain_lm2 <- lm(Estimated_Pain ~ Gender + Patient_Gender, data = pain)
summary(pain_lm2)


# Question 8

predict(pain_lm2, newdata = data.frame(Gender = c("Man", "Woman"), 
                                       Patient_Gender = c("Woman", "Man")))


# Question 9

pain_lm3 <- lm(Estimated_Pain ~ Gender + Patient_Gender + Gender*Patient_Gender, 
               data = pain)
summary(pain_lm3)


# Question 10

table(pain$Gender)


# Question 11

predict(pain_lm3, newdata = data.frame(Gender = c("Man", "Man"), 
                                       Patient_Gender = c("Woman", "Man")))
predict(pain_lm3, newdata = data.frame(Gender = c("Woman", "Woman"), 
                                       Patient_Gender = c("Woman", "Man")))


# Question 12

anova(pain_lm3)
anova(pain_lm2)
pf(7.814, 2, 103, lower.tail = FALSE)
