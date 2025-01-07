# setwd("C:\\Studium\\Master\\Semester2\\DataLiteracy\\project\\DataLiteracyWS24")
# getwd()

data <- read.csv("student_lifestyle_dataset.csv")
library(dplyr)
library(ggplot2)

# Data exploration
View(data)
summary(data)
str(data)
head(data)

# Data pre-processing
data 
# %>% 

data$high_preformer <- ifelse(data$GPA >= 3.5, 1, 0)
high_p <- data[data$high_preformer == 1,]
mean(high_p$Study_Hours_Per_Day)

data$Stress_Level <- ifelse(data$Stress_Level == "Low", 0, 
                                             ifelse(data$Stress_Level == "Moderate", 1, 
                                                    ifelse(data$Stress_Level == "High", 2, 7)))

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


