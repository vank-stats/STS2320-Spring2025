# STS 2320 - Activity 01

# Question 1 - Read in data
# Put the data in the same file as your script 
# Then set working directory to source file location

library(readxl)

penguins <- read_xlsx("gentoo_penguins.xlsx")


# Question 2 - View data

View(penguins)


# Question 3 - Scatter plot of body mass vs. flipper length

library(ggplot2)

ggplot(penguins) +
  geom_point(aes(x = body_mass_g, y = flipper_length_mm)) +
  labs(x = "Body mass (grams)",
       y = "Flipper length (mm)",
       title = "Comparing Gentoo Penguin Body Mass and Flipper Length")


# Question 4 - Correlation with hypothesis test

cor.test(penguins$body_mass_g, penguins$flipper_length_mm)


# Question 5 - Creating new sex2 variable

library(dplyr)

penguins <- penguins |>
  mutate(sex2 = ifelse(is.na(sex), "missing", sex))

table(penguins$sex2)


# Question 6 - Summary of body mass and flipper length by sex2 values

tapply(penguins$body_mass_g, penguins$sex2, summary)
tapply(penguins$flipper_length_mm, penguins$sex2, summary)


# Question 7 - Updated scatterplot with points colored by sex2 values

ggplot(penguins) +
  geom_point(aes(x = body_mass_g, y = flipper_length_mm, color = sex2)) +
  labs(x = "Body mass (grams)",
       y = "Flipper length (mm)",
       title = "Comparing Gentoo Penguin Body Mass and Flipper Length")


# Question 8 - Box plots of body mass by sex2 values

ggplot(penguins) +
  geom_boxplot(aes(x = body_mass_g, y = sex2)) +
  labs(x = "Body mass (grams)",
       y = "Sex of penguin",
       title = "Gentoo penguin body masses by sex")
