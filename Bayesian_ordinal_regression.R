# setwd("C:\\Studium\\Master\\Semester2\\DataLiteracy\\project\\DataLiteracyWS24")
# getwd()

setwd("/Desktop/Data Literacy/Project/DataLiteracyWS24/Bayesian_ordinal_regression.R")
data <- read.csv("student_lifestyle_dataset.csv")
library(dplyr)
library(ggplot2)
# For the Bayesian ordinal logistic regression
library(brms)
library(brant)
library(rstan)
#install.packages("rtools")

# Data exploration
View(data)
summary(data)
str(data)
head(data)

# Data pre-processing
data 
# %>% 

# Select the upper 25% of the students as high performers
hist(data$GPA)
GPA_high_achiever <- quantile(data$GPA, 0.75)
# -> delivers 3.33 as the 75th percentile

# Create a new column for high performers
data$high_preformer <- ifelse(data$GPA >= GPA_high_achiever, 1, 0)
mean(high_p$Study_Hours_Per_Day)

# Convert the stress level to a numeric variable
data$Stress_Level <- ifelse(data$Stress_Level == "Low", 0, 
                                             ifelse(data$Stress_Level == "Moderate", 1, 
                                                    ifelse(data$Stress_Level == "High", 2, 7)))


# Filter the data for high performers
high_p <- data[data$high_preformer == 1,]

# Create a new column for the sum of social and extracurricular hours
high_p <- mutate(high_p, social_and_extrac = Social_Hours_Per_Day + Extracurricular_Hours_Per_Day)

# Filter the data for low performers
average_stress_per_social_hours_low_performing <- data %>% 
  filter(GPA >= 0 & GPA <= 3.2) %>%
  group_by(Social_Hours_Per_Day) %>%
  summarise(average_stress = mean(Stress_Level, na.rm = TRUE))

View(average_stress_per_social_hours_low_performing)

average_stress_per_social_hours_high_performing <- data %>% 
  filter(GPA <= 4 & GPA > 3.2) %>%
  group_by(Social_Hours_Per_Day) %>%
  summarise(average_stress = mean(Stress_Level, na.rm = TRUE))

View(average_stress_per_social_hours_high_performing)

average_stress_filtered <- data %>%
  group_by(Social_Hours_Per_Day) %>%
  summarise(average_stress = mean(data$Stress_Level, na.rm = TRUE))

# Data visualization
plot(data$Study_Hours_Per_Day, data$Sleep_Hours_Per_Day, xlab = "Study", ylab = "Sleep",
     main = "Study and sleep", col = "blue")

plot(data$Study_Hours_Per_Day, data$GPA, xlab = "Study", ylab = "GPA",
     main = "Study and GPA", col = "blue")

plot(data$Social_Hours_Per_Day, data$GPA, xlab = "Social", ylab = "GPA",
     main = "Social and GPA", col = "blue", pch = 4)


x <- as.numeric(average_stress_filtered$Social_Hours_Per_Day)
y_low <- as.numeric(average_stress_per_social_hours_low_performing$average_stress)
y_high <- as.numeric(average_stress_per_social_hours_high_performing$average_stress)



# Plot the social hours over average stress levels for low and high performers
plot(x, y_low, type = "b", col = "blue", pch = 19, lty = 1,
     xlab = "Social Hours per Day", ylab = "Average Stress Level",
     main = "Average Stress Level by Social Hours (Low Performing)")

plot(x, y_high, type = "b", col = "blue", pch = 19, lty = 1,
     xlab = "Social Hours per Day", ylab = "Average Stress Level",
     main = "Average Stress Level by Social Hours (High Performing)")


# Model fitting

# Model 1: Predictors: Social Hours 
# -> Not a good fit, diverges
model1 <- brm(factor(Stress_Level, ordered = TRUE) ~ Social_Hours_Per_Day, data = high_p, family = cumulative())
summary(model1)

# Model 2: Predictors: Study Hours 
# -> More study, more stress
model2 <- brm(factor(Stress_Level, ordered = TRUE) ~ Study_Hours_Per_Day, data = high_p, family = cumulative(link = "logit"))
summary(model2)
plot(model2)
pp_check(model2)

# Model 3: Predictors: Physical Activity 
# -> More phys-ac, less stress (-95% interval is veeery close to zero though)
model3 <- brm(factor(Stress_Level, ordered = TRUE) ~ Physical_Activity_Hours_Per_Day, data = high_p, family = cumulative(link = "logit"))
summary(model3)
plot(model3)
pp_check(model3)

# Model 4: Predictors: Study Hours and Physical Activity 
# -> Physical Activity seems to be not significant
model4 <- brm(factor(Stress_Level, ordered = TRUE) ~ Study_Hours_Per_Day + Physical_Activity_Hours_Per_Day, data = high_p, family = cumulative(link = "logit"))
summary(model4)
plot(model4)
pp_check(model4)

# Model 5: Predictors: Study Hours and Physical Activity and Social Hours 
# -> both Physical Activity and Social Hours are not good predictors (95% interval includes zero for both of them)
model5 <- brm(factor(Stress_Level, ordered = TRUE) ~ Study_Hours_Per_Day + Physical_Activity_Hours_Per_Day + Social_Hours_Per_Day, data = high_p, family = cumulative(link = "logit"))
summary(model5)
plot(model5)
pp_check(model5)

# Model 6: Predictors: Sleep Hours
# -> The effect of sleep seems not to be significant (95% interval includes zero)
model6 <- brm(factor(Stress_Level, ordered = TRUE) ~ Sleep_Hours_Per_Day, 
                     data = high_p, family = cumulative(link = "logit"))
summary(model6)
plot(model6)
pp_check(model6)

# Model 7: Predictors: Sleep Hours and Study Hours
# -> more study more stress but more sleep less stress
model7 <- brm(factor(Stress_Level, 
                     ordered = TRUE) ~ Sleep_Hours_Per_Day + Study_Hours_Per_Day, 
                     data = high_p, family = cumulative(link = "logit"))
summary(model7)
plot(model7)
pp_check(model7)


# # t-value from the output
# t_value <- -1.171
# 
# # Standard normal cumulative distribution function (CDF)
# p_value <- 2 * (1 - pnorm(abs(t_value)))
# 
# p_value
# 
# (ctable <- coef(summary(model)))
# 
# ## calculate and store p values
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# 
# ## combined table
# (ctable <- cbind(ctable, "p value" = p))


# For some reason I have to run this lines in my console so that the models compile, please ignore

# Sys.setenv(CC = "clang")
# Sys.setenv(CXX = "clang++")
# Sys.setenv(CFLAGS = "-isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX14.4.sdk")
# Sys.setenv(CXXFLAGS = "-isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX14.4.sdk")
# Sys.setenv(LDFLAGS = "-isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX14.4.sdk")


