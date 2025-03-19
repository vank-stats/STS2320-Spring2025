# Read in voucher data
# Remember to put script and data in same folder. Then set working directory
#   to Source File Location (that same folder)

library(readxl) # to read in data
library(ggplot2) # for graph in question 1
library(gglm) # for diagnostic plots in questions 4 - 5
library(broom) # for residuals, leverage, influence in question 6
library(dplyr) # to create variables with mutate() function

vouchers <- read_xlsx("AZ_Vouchers.xlsx")


# Question 1 (only first two lines are needed)

ggplot(vouchers, aes(x = AvgIncome, y = VouchersPer100)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()


# Question 2

vouchers_lm <- lm(VouchersPer100 ~ AvgIncome, data = vouchers)
summary(vouchers_lm)


# Questions 4 and 5

gglm(vouchers_lm)


# Question 6

vouchers_aug <- augment(vouchers_lm)
View(vouchers_aug)


# Question 7

vouchers <- mutate(vouchers, log_vouchers = log(VouchersPer100))
log_vouchers_lm <- lm(log_vouchers ~ AvgIncome, data = vouchers)
summary(log_vouchers_lm)


# Question 8

ggplot(vouchers, aes(x = AvgIncome, y = log_vouchers)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

gglm(log_vouchers_lm)


# Question 9

log_vouchers_aug <- augment(log_vouchers_lm)
View(log_vouchers_aug)


# Question 10

preds <- predict(log_vouchers_lm, 
                 newdata = data.frame(AvgIncome = c(50000,
                                                    150000)))
preds
exp(preds)


# Question 11

preds_pi <- predict(log_vouchers_lm, 
                    newdata = data.frame(AvgIncome = c(50000,
                                                       150000)),
                    interval = "prediction")
preds_pi
exp(preds_pi)


# Questions 12 and 13 - no additional code


# Question 14

vouchers <- mutate(vouchers, log_income = log(AvgIncome))

vouchers_doublelog_lm <- lm(log_vouchers ~ log_income, data = vouchers)

ggplot(vouchers, aes(x = log_income, y = log_vouchers)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

gglm(vouchers_doublelog_lm)
