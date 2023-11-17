library('neuralnet')
library('ggplot2')
library('dplyr') # for resampling
library('caret') # for confusion matrix

data <- read.table("Datasets/healthcare-dataset-stroke-data.csv", header = TRUE, sep = ",")
header_names <- names(data)
age <- data$age
bmi <- data$bmi
gender <- data$gender
heart_disease <- data$heart_disease
hypertension <- data$hypertension
avg_glucose_level <- data$avg_glucose_level
stroke <- data$stroke

training_set <- data.frame(
  age,
  bmi,
  #gender,
  heart_disease,
  hypertension,
  avg_glucose_level,
  stroke
)
subframe <- data.frame(
  age,
  bmi,
  #gender,
  heart_disease,
  hypertension,
  avg_glucose_level
)
training_set[is.na(training_set)] <- 0
subframe[is.na(subframe)] <- 0
rnd <- sample(1:150, 1)
set.seed(rnd)
mysample = sample_n(training_set, 100, replace=TRUE)
print(mysample)
myspecies = mysample[5:5]
print(myspecies)

form <- stroke~bmi+age+heart_disease+hypertension+avg_glucose_level
mytest <- neuralnet(form, mysample,
                    hidden = 3, linear.output = FALSE)
print(mytest)
print(plot(mytest))
mypred <- predict(mytest, subframe, rep = 1, all.units = FALSE)
print(round(mypred))
library(data.table)
print(max.col(mypred)) # return column with maximum value
