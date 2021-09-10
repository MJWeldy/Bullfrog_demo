library(keras)
k_backend()
library(tidyverse)

model %>% compile(optimizer = optimizer_adam(decay = 1e-5),
                  loss = "categorical_crossentropy",
                  metrics = "accuracy")

history <- fit(model, x = train$X, y = train$Y,
               batch_size = 16, epochs = 10,
               validation_data = list(val$X, val$Y))