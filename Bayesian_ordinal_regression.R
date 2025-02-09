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
library(reshape2)
library(viridis)
#install.packages("rtools")

# Data exploration
View(data)
summary(data)
str(data)
head(data)

# Data pre-processing
data 
# %>% 

cor_matrix <- cor(data[2:8])
print(cor_matrix)

# Reshape the correlation matrix into long format
cor_melted <- melt(cor_matrix)

# Create a new column for high performers
data$high_preformer <- ifelse(data$GPA >= 3.5, 1, 0)
mean(high_p$Study_Hours_Per_Day)

# Convert the stress level to a numeric variable
data$Stress_Level <- ifelse(data$Stress_Level == "Low", 0, 
                                             ifelse(data$Stress_Level == "Moderate", 1, 
                                                    ifelse(data$Stress_Level == "High", 2, 7)))

# Plot the heatmap
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis(option = "cividis", direction = 1) +  # 'D' is the default perceptually uniform colormap
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "Feature", y = "Feature")


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
bprior <- c(prior(normal(-10,1), class = b, coef = Social_Hours_Per_Day))

model1 <- brm(factor(Stress_Level, ordered = TRUE) ~ Social_Hours_Per_Day, data = high_p, family = cumulative())
summary(model1)
plot(model1)
get_prior(factor(Stress_Level, ordered = TRUE) ~ Social_Hours_Per_Day, data = high_p, family = cumulative(), prior = bprior)
# Model 2: Predictors: Study Hours 
# -> More study, more stress
model2 <- brm(factor(Stress_Level, ordered = TRUE) ~ Study_Hours_Per_Day, 
              data = high_p, 
              family = cumulative(link = "logit"),
)
summary(model2)
plot(model2)
pp_check(model2)

get_prior(factor(Stress_Level, ordered = TRUE) ~ Study_Hours_Per_Day, 
          data = high_p, 
          family = cumulative(link = "logit"))

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

invlogit <- function(x){
  exp(x) / (1 + exp(x))
}




P_Stress_0 <- function (intercept1, intercept2, W, X) {
  invlogit(intercept1 - W * X)
}
P_Stress_1 <- function (intercept1, intercept2, W, X) {
  invlogit(intercept2 - W * X) - P_Stress_0(intercept1, intercept2, W, X)
}
P_Stress_2 <- function (intercept1, intercept2, W, X) {
  1 - P_Stress_1(intercept1, intercept2, W, X) - P_Stress_0(intercept1, intercept2, W, X)
}

P_Stress <- function(level, intercept1, intercept2, W, X) {
  if (level == 0) {
    return(P_Stress_0(intercept1, intercept2, W, X))
  } else if (level == 1) {
    return(P_Stress_1(intercept1, intercept2, W, X))
  } else if (level == 2) {
    return(P_Stress_2(intercept1, intercept2, W, X))
  } else {
    stop("Invalid stress level. Please provide one of the levels 1, 2 or 3.")
  }
}


post <- posterior_samples(model1)

post2 <- posterior_samples(model2)

add_prob_columns <- function(post, predictor, hours) {
  # Create a helper function to generate the probability columns dynamically
  for (level in 0:2) {
    for (hour in hours) {
      # Create the column name dynamically, e.g., P_0_4, P_1_6, etc.
      column_name <- paste("P", level, hour, sep = "_")
      # Use the P_Stress function for each stress level and hour, add the column to post
      post <- post %>%
        mutate(!!column_name := P_Stress(level, 
                                         `b_Intercept[1]`, `b_Intercept[2]`, 
                                         !!sym(predictor), hour))
    }
  }
  result <- post[, 9:ncol(post)]
  return(result)
}

probs_4 <- add_prob_columns(post, "b_Social_Hours_Per_Day", 6)
probs <- add_prob_columns(post, "b_Social_Hours_Per_Day", 6)
probs2 <- add_prob_columns(post2, "b_Study_Hours_Per_Day", 6)

# post %>%
#   mutate(P_0_4 = P_Stress_0(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 4),
#          P_0_6 = P_Stress_0(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 6),
#          P_0_8 = P_Stress_0(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 8)) ->
#   post
# 
# post %>%
#   mutate(P_1_4 = P_Stress_1(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 4),
#          P_1_6 = P_Stress_1(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 6),
#          P_1_8 = P_Stress_1(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 8)) ->
#   post
# 
# post %>%
#   mutate(P_2_4 = P_Stress_2(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 4),
#          P_2_6 = P_Stress_2(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 6),
#          P_2_8 = P_Stress_2(`b_Intercept[1]`, `b_Intercept[2]`, b_Study_Hours_Per_Day, 8)) ->
#   post

mean_P_0_8 <- mean(post$P_0_8)
print(mean_P_0_8)

mean_P_0_6 <- mean(post$P_0_6)
print(mean_P_0_6)

mean_P_0_4 <- mean(post$P_0_4)
print(mean_P_0_4)

mean_P_1_8 <- mean(post$P_1_8)
print(mean_P_1_8)

mean_P_1_6 <- mean(post$P_1_6)
print(mean_P_1_6)

mean_P_1_4 <- mean(post$P_1_4)
print(mean_P_1_4)

mean_P_2_8 <- mean(post$P_2_8)
print(mean_P_2_8)

mean_P_2_6 <- mean(post$P_2_6)
print(mean_P_2_6)

mean_P_2_4 <- mean(post$P_2_4)
print(mean_P_2_4)



###### Discouraged :(
ggplot(probs, aes(P_0_6)) +
  geom_density(color = "blue") + 
  geom_density(aes(P_1_6),
               color = "red")

ggplot(probs2, aes(P_0_6)) +
  geom_density(color = "blue") +
  geom_density(aes(P_1_6),
               color = "red")
  

# Basically the "probability of the probability of being in Stress level 0 (blue)
# or level 1 (red), given that the student studies for 6 hours a day". Each entry
# in the column $P_0_6 in the vector post gives the probability of being in level
# 0 given that the student studies for 6 hours a day, given the parameters of this
# entry. The plot then shows how often each probability occurs. 

## Given that a student studies for 5 hours per day, it is very likely that the
## true probability of him having stress level 0 is greater than 95%.
ggplot(add_prob_columns(post, "b_Social_Hours_Per_Day", c(1,2,3,4,5,6,7,8)), aes(P_0_4)) +
  geom_density(color = "blue") +
  geom_density(aes(P_0_6),
               color = "red")+
  geom_density(aes(P_0_8),
               color = "green")+
  geom_density(aes(P_0_1),
               color = "purple")+
  geom_density(aes(P_0_2),
               color = "yellow")+
  geom_density(aes(P_0_3),
               color = "brown")+
  geom_density(aes(P_0_5),
               color = "black")+
  geom_density(aes(P_0_7),
               color = "orange")


ggplot(add_prob_columns(post, "b_Social_Hours_Per_Day", c(1,2,3,4,5,6,7,8)), aes(P_1_4)) +
  geom_density(color = "blue") +
  geom_density(aes(P_1_6),
               color = "red")+
  geom_density(aes(P_1_8),
               color = "green")

ggplot(add_prob_columns(post, "b_Social_Hours_Per_Day", c(1,2,3,4,5,6,7,8)), aes(P_2_4)) +
  geom_density(color = "blue") +
  geom_density(aes(P_2_6),
               color = "red")+
  geom_density(aes(P_2_8),
               color = "green")




# 
# ggplot(add_prob_columns(post2, "b_Study_Hours_Per_Day", c(1,2,3,4,5,6,7,8)), aes(P_0_4)) +
#   geom_density(color = "blue") +
#   geom_density(aes(P_0_6),
#                color = "red")+
#   geom_density(aes(P_0_8),
#                color = "green")
# 
# ggplot(add_prob_columns(post2, "b_Study_Hours_Per_Day", c(1,2,3,4,5,6,7,8)), aes(P_1_4)) +
#   geom_density(color = "blue") +
#   geom_density(aes(P_1_6),
#                color = "red")+
#   geom_density(aes(P_1_8),
#                color = "green")
# 
# ggplot(add_prob_columns(post2, "b_Study_Hours_Per_Day", c(1,2,3,4,5,6,7,8)), aes(P_2_4)) +
#   geom_density(color = "blue") +
#   geom_density(aes(P_2_6),
#                color = "red")+
#   geom_density(aes(P_2_8),
#                color = "green")


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


