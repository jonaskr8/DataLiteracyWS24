setwd("C:/Users/schae/OneDrive/Documents/Uni/WiSe24_25/DataLiteracy/DataLiteracyWS24")
# getwd()

data <- read.csv("student_lifestyle_dataset.csv")
library(dplyr)
library(ggplot2)
# For the ordinal logistic regression
library(MASS)

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
social <- mutate(data, social_and_extr = Social_Hours_Per_Day + Extracurricular_Hours_Per_Day + Physical_Activity_Hours_Per_Day)

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

# I want to view all participant's stress level plotted (no average, x axis is simply participant no.)
plot(data$Stress_Level, xlab = "Participant", ylab = "Stress Level",
     main = "Stress Level of all participants", col = "blue")

# Data visualization
plot(data$Study_Hours_Per_Day, data$Sleep_Hours_Per_Day, xlab = "Study", ylab = "Sleep",
     main = "Study and sleep", col = "blue")

plot(data$Study_Hours_Per_Day, data$GPA, xlab = "Study", ylab = "GPA",
     main = "Study and GPA", col = "blue")

plot(data$Sleep_Hours_Per_Day, data$GPA, xlab = "Sleep", ylab = "GPA",
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

model <- polr(factor(Stress_Level) ~ social_and_extrac, high_p, Hess = TRUE)
summary(model)

model1 <- polr(factor(Stress_Level) ~ Sleep_Hours_Per_Day, high_p, Hess = TRUE)
summary(model1)

model2 <- polr(factor(Stress_Level) ~ social_and_extr, social, Hess = TRUE)
summary(model2)

model3 <- polr(factor(Stress_Level) ~ Study_Hours_Per_Day, social, Hess = TRUE)
summary(model3)

# comparison of stress levels - social+extrac+physical between high performers and all students
plot(factor(high_p$Stress_Level), high_p$social_and_extr, xlab = "Stress Level", ylab = "social, extracur and physical hours/day", ylim = c(0,14))
plot(factor(social$Stress_Level), social$social_and_extr, xlab = "Stress Level", ylab = "social, extracur and physical hours/day", ylim = c(0,14))

# t-value from the output
t_value <- -1.171

# Standard normal cumulative distribution function (CDF)
p_value <- 2 * (1 - pnorm(abs(t_value)))

p_value

(ctable <- coef(summary(model)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

