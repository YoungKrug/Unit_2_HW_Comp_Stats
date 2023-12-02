library("tensorflow")
library("tfdatasets")
library("keras")
library("caret")
library("tidyverse")
library("tidymodels")
library("GGally")
library("skimr")

#install_tensorflow()
#install_tensorflow(method="conda", envname = "Unit_2_HW_Comp_Stats")
#install.packages("keras")
fashion_mnist <- dataset_fashion_mnist() # 70,000 low resolution grayscale images of clothing
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat',
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')  # class names for plotting