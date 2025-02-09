# DataLiteracy Project: Predicting Stress Levels of Students based od their Daily Habits

## About the Dataset
The Dataset used for this Project is the [students lifesyle database](https://www.kaggle.com/datasets/steve1215rogg/student-lifestyle-dataset), which covers 2000 Students. 
It contains information about the lifestyle patterns of students with records the daily amout of studying, extracurricular activities, sleep, socialising and physical 
activity together with the students GPA and reported stress level.

## Packages
In order to run the code of this project these packages have to be installed
```
library(dplyr)
library(ggplot2)
# For the Bayesian ordinal logistic regression
library(brms)
library(brant)
library(rstan)
library(reshape2)
library(viridis)
```

## Data Pre-Processing
Establish threshold to differentiate between high and lower performing students.\
Also transforming the stress level valus "Low" "Moderate" and "High" to the numerical values 0, 1, 2 respectively. 

## Model
Using the `brm` function from the `brms` package to perform bayesian inference with ordinal logistig regression with the family `cumulative(link = "logit")`.
Using different variables of the dataset in different combinations as predictors. Example:
```
model <- brm(factor(Stress_Level, ordered = TRUE) ~ Social_Hours_Per_Day, data = high_p, family = cumulative(link = "logit"))
pp_check(model)
```
Performing `pp_check()` on the model to perform posterior predictive checks on the models to see if the model predicts the data accuratly.

Computing the probability of being in a given stress level based on intercepts and student habits `W`
```
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
```

