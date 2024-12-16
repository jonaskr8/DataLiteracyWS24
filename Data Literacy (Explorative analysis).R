setwd("/Users/nicolasdelgadolozano/Desktop/Data Literacy/Project")
getwd()

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


# Data visualization
ggplot(data$Study_Hours_Per_Day, data$Sleep_Hours_Per_Day, xlab = "Study", ylab = "Sleep",
     main = "Study and sleep", col = "blue")

ggplot(data$Study_Hours_Per_Day, data$GPA, xlab = "Study", ylab = "GPA",
     main = "Study and sleep", col = "blue")

plot(data$Social_Hours_Per_Day, data$GPA, xlab = "Study", ylab = "GPA",
     main = "Study and sleep", col = "blue", pch = 4)




