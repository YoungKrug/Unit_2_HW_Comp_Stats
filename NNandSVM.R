
library('kernlab')
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
  #bmi,
  #gender,
  heart_disease,
  hypertension,
  avg_glucose_level,
  stroke
)
training_set[is.na(training_set)] <- 0
subframe[is.na(subframe)] <- 0
rnd <- sample(1:150, 1)
set.seed(rnd)
mysample <- sample_n(subframe, 100, replace=TRUE)
# print(mysample)
myspecies <- mysample[,5]
print(myspecies)

form <- stroke~age+heart_disease+hypertension+avg_glucose_level
mytest <- svm(stroke~age+heart_disease+hypertension+avg_glucose_level,
              mysample ,probability = TRUE, type = "C-classification", kernel = "polynomial", degree = 2)
mytest2 <- neuralnet(stroke~age+heart_disease+hypertension+avg_glucose_level, mysample,
                    hidden = 3, linear.output = FALSE)
#compare / contrast

mypred <- predict(mytest, subframe, type = 'response')
mypred2 <- predict(mytest2, subframe, rep = 1, all.units = FALSE)
print("SVM")
# confusion matrix
#print(as.factor(mypred))
#print(as.factor(stroke))
mymatrix <- confusionMatrix(as.factor(mypred), as.factor(stroke))
print(mymatrix)

print("NeuralNet")
#mypred <- knn(mysample, subframe, myspecies, k=3, l = 0, prob = FALSE, use.all = TRUE)
#print(max.col(mypred2))
St_factor <- as.factor(stroke)
#print(as.integer(St_factor))
St_integer <- as.integer(St_factor)
mymatrix <- confusionMatrix(as.factor(max.col(mypred2)), as.factor(St_integer))
print(mymatrix)
