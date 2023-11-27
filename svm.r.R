
library('e1071')
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
mysample = sample_n(subframe, 100, replace=TRUE)
# print(mysample)
myspecies = mysample[,5]
 print(myspecies)

form <- stroke~age+heart_disease+hypertension+avg_glucose_level
# mytest <- svm(stroke~age+heart_disease+hypertension+avg_glucose_level,
#             mysample ,probability = TRUE, type = "C-classification", kernel = "linear")
mytest <- svm(stroke~age+heart_disease+hypertension+avg_glucose_level,
              mysample ,probability = TRUE, type = "C-classification", kernel = "polynomial", degree = 2)

mypred <- predict(mytest, subframe, probability = FALSE, decision.values = TRUE)
myplot1 <-ggplot(mysample, aes(age, avg_glucose_level, colour = stroke)) + geom_point()
myplot2 <-ggplot(mysample, aes(hypertension, heart_disease, colour = stroke)) + geom_point()
myplot3 <-ggplot(subframe, aes(age, avg_glucose_level, colour = stroke)) + geom_point()
myplot4 <-ggplot(subframe, aes(hypertension, heart_disease, colour = stroke)) + geom_point()
mySVM <- summary(mytest)
print(mySVM)
myplot5 <-ggplot(subframe, aes(age, avg_glucose_level, colour = mypred)) + geom_point()
myplot6 <-ggplot(subframe, aes(hypertension, heart_disease, colour = mypred)) + geom_point()
library('grid')
pushViewport(viewport(layout = grid.layout(3, 2)))
print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(myplot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(myplot5, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(myplot6, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
# confusion matrix
print(as.factor(mypred))
print(as.factor(stroke))
mymatrix <- confusionMatrix(as.factor(mypred), as.factor(stroke))
print(mymatrix)
