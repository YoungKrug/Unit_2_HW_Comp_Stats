library("tensorflow")
library("tfdatasets")
library("keras")
library("caret")
library("tidyverse")
library("tidymodels")
library('tictoc')

#install_tensorflow()
#install_tensorflow(method="conda", envname = "Unit_2_HW_Comp_Stats")
#install.packages("keras")

cifar_data <- dataset_cifar10() #
c(train_images, train_labels) %<-% cifar_data$train
c(test_images, test_labels) %<-% cifar_data$test
class_names <- c('airplane', 'automobile','bird', 'cat','deer',
                'dog','frog','horse','ship','truck')# class names for plotting
# check data
print("number training images and labels")
print(dim(train_images))
print(dim(train_labels))
print("print some labels")
print(train_labels[1:20])
print("number testing images and labels")
print(dim(test_images))
print(dim(test_labels))

train_images <- array_reshape(train_images, c(dim(train_images)[1], dim(train_images)[2], dim(train_images)[3], 3))
train_images <- train_images / 255.0

# Reshape and normalize x_test
test_images <- array_reshape(test_images, c(dim(test_images)[1], dim(test_images)[2], dim(test_images)[3], 3))
test_images <- test_images / 255.0
# build deep net model
tic("time elapsed")
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(32, 32, 3)) %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 20, activation = 'softmax')

# compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# train model and summarize it
model %>% fit(train_images, train_labels, epochs = 50, verbose = 2)
print(model)

# make probabilistic predictions
predictions <- model %>% predict(test_images)
print(predictions[1, ]) # print first prediction
print(which.max(predictions[1, ])) # print which of 10 classes wins
# get class predictions
class_pred <- model %>% predict(test_images) %>% k_argmax() %>%
   as.integer()

# plot some classifications
mymatrix <- confusionMatrix(as.factor(class_pred), as.factor(test_labels))
print(mymatrix)
toc() # print time elapsed

# The neural net and the ada had a higher CI, but this is a image classification dataset.
# Introduces more error, and has more data. It also runs much much longer.