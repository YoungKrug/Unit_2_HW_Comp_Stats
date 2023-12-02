library("tensorflow")
library("tfdatasets")
library("keras")
library("caret")
library("tidyverse")
library("tidymodels")
library("GGally")
library("skimr")

# c(c(x_train, y_train), c(x_test, y_test)) %<-% keras::dataset_mnist()
# x_train %<>% { . / 255 }
# x_test  %<>% { . / 255 }
#
# train_ds <- list(x_train, y_train) %>%
#   tensor_slices_dataset() %>%
#   dataset_shuffle(10000) %>%
#   dataset_batch(32)
#
# test_ds <- list(x_test, y_test) %>%
#   tensor_slices_dataset() %>%
#   dataset_batch(32)

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

# dataSet_spread <- training_set %>%
#   spread(key = age, value = lifeExp)
# library("recipes")
# dataset <- recipe(age ~ ., training_set) %>%
#   step_num2factor(origin, levels = c("USA", "Europe", "Japan")) %>%
#   step_dummy(origin, one_hot = TRUE) %>%
#   prep() %>%
#   bake(new_data = NULL)
#
#

training_set <- na.omit(training_set)

split <- initial_split(training_set, 0.8)
train_dataset <- training(split)
test_dataset <- testing(split)

train_dataset %>%
  select(age, heart_disease, avg_glucose_level, hypertension) %>%
  GGally::ggpairs()

skimr::skim(train_dataset)