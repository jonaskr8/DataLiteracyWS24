setwd("/Users/nicolasdelgadolozano/Desktop/Data Literacy/Project")
getwd()

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

mean_study_hours <- data %>%
  mutate(high_preformer = ifelse(GPA >= 3.5, 1, 0)) %>%
  filter(high_preformer == 1) %>%
  summarise(mean_study_hours = mean(Study_Hours_Per_Day, na.rm = TRUE))



data 
  mutate(social_and_extrac = Social_Hours_Per_Day + Extracurricular_Hours_Per_Day) %>%
  ggplot(aes(x = social_and_extrac, y = GPA)) +
  geom_point()


# Model fitting 
  
  #polr()



