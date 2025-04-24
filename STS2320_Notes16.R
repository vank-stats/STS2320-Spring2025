# STS 2320 - Notes 16 Code

# First set your working directory so the file path works
# Load appropriate packages

# Read mlb draft data into R
# My data was in a Data folder in my working directory. Yours may not be.

mlb <- read.csv("Data/mlb_draft.csv")


# Logistic regression predicting majors with Type and Position

mlb_log <- glm(majors ~ Type + Position, family = "binomial", data = mlb)
summary(mlb_log)


# Confidence interval for coefficients

confint(mlb_log)


# Confidence interval for odds ratio of coefficients

exp(confint(mlb_log))


# Predicted probabilities for category combinations in our data

predict(mlb_log, type = "response", 
        newdata = data.frame(Type = c("HS", "HS", "4Yr", "4Yr"),
                             Position = c("hitter", "pitcher", "hitter", "pitcher")))
