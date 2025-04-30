# STS 2320 - Notes 17 Code

# First set your working directory so the file path works
# Load appropriate packages

# Read mlb draft data into R
# My data was in a Data folder in my working directory. Yours may not be.

mlb <- read.csv("Data/mlb_draft.csv")


# Logistic regression predicting majors with OvPck, Type, and Position

mlb_log <- glm(majors ~ OvPck + Type + Position, family = "binomial", data = mlb)
summary(mlb_log)


# Confidence interval for coefficients and for odds ratios of coefficients

confint(mlb_log)
exp(confint(mlb_log))


# Predicted probabilities for two players

predict(mlb_log, type = "response", 
        newdata = data.frame(OvPck = c(15, 29),
                             Type = c("4Yr", "HS"),
                             Position = c("pitcher", "hitter")))


# Fit reduced model to use in LRT
# Note: Some values were removed in the larger model. These also need to be
#   removed in the smaller model. I am using filter() from the dplyr package
#   to do this.

library(dplyr)
mlb_sub <- filter(mlb, !is.na(Type))

mlb_log_reduced <- glm(majors ~ OvPck, family = "binomial", data = mlb_sub)
summary(mlb_log_reduced)


# Conduct Likelihood Ratio Test (need lmtest package)

library(lmtest)

lrtest(mlb_log_reduced, mlb_log)


# Backward selection from large model (uses AIC)

mlb_large <- glm(majors ~ OvPck + I(OvPck^2) + Type + Position + 
                   OvPck*Position + OvPck*Type,
                 family = "binomial",
                 data = mlb)

step(mlb_large, direction = "backward")


# Hosmer-Lemeshow Goodness of Fit Test
# Note: It may not split data into exact same categories as SAS

library(ResourceSelection)

mlb_picktype <- glm(majors ~ OvPck + Type, family = "binomial", data = mlb)

hoslem.test(mlb_picktype$y, fitted(mlb_picktype))
