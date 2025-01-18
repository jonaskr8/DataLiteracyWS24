# setwd("C:\\Studium\\Master\\Semester2\\DataLiteracy\\project\\DataLiteracyWS24")
# getwd()

data <- read.csv("student_lifestyle_dataset.csv")
library(dplyr)
library(ggplot2)
# For the Bayesian ordinal logistic regression
library(brms)

# Data exploration
View(data)
summary(data)
str(data)
head(data)

# Data pre-processing
data 
# %>% 

data$high_preformer <- ifelse(data$GPA >= 3.5, 1, 0)
mean(high_p$Study_Hours_Per_Day)

data$Stress_Level <- ifelse(data$Stress_Level == "Low", 0, 
                                             ifelse(data$Stress_Level == "Moderate", 1, 
                                                    ifelse(data$Stress_Level == "High", 2, 7)))

high_p <- data[data$high_preformer == 1,]
high_p <- mutate(high_p, social_and_extrac = Social_Hours_Per_Day + Extracurricular_Hours_Per_Day)


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



# Plot the data
plot(x, y_low, type = "b", col = "blue", pch = 19, lty = 1,
     xlab = "Social Hours per Day", ylab = "Average Stress Level",
     main = "Average Stress Level by Social Hours (Low Performing)")

plot(x, y_high, type = "b", col = "blue", pch = 19, lty = 1,
     xlab = "Social Hours per Day", ylab = "Average Stress Level",
     main = "Average Stress Level by Social Hours (High Performing)")


# Model fitting

model1 <- brm(factor(Stress_Level, ordered = TRUE) ~ Social_Hours_Per_Day, data = high_p, family = cumulative())
summary(model1)

model2 <- brm(Stress_Level ~ Study_Hours_Per_Day, data = high_p)
summary(model2)
plot(model2)
pp_check(model2)
# 
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

