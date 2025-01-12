# DataLiteracy Project: Predicting Stress Levels of Students based od their Daily Habits

## About the Dataset
The Dataset used for this Project is the [students lifesyle datase](https://www.kaggle.com/datasets/steve1215rogg/student-lifestyle-dataset), which covers 2000 Students. It contains Information about the lifestyle patterns of students with records the daily amout of studying, extracurricular activities, sleep, socialising and physical activity together with the students GPA and reported stress level.

## Packages
```
library(dplyr)
library(ggplot2)
library(MASS)
```

## Model
We are using a ordinal linerar regression modle to determine the stress level of the students based on their daily habits. We do this with the `polr` function.
...