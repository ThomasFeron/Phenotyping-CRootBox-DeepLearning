library(keras)
library(dplyr)
library(ggplot2)
library(tidyr)

#install_keras()
use_session_with_seed(1,disable_parallel_cpu = FALSE)
data = iris[sample(nrow(iris)),]

y = data[, "Species"]
x = data[,1:4]

x = as.matrix(apply(x, 2, function(x) (x-min(x))/(max(x) - min(x))))

levels(y) = 1:length(y)
y = to_categorical(as.integer(y) - 1 , num_classes = 3)

model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x), units = 10, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

fit = model %>%
  fit(
    x = x,
    y = y,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.3,
    epochs = 100
  )

plot(fit)

