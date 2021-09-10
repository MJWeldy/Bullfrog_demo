library(keras)
k_backend()
library(tidyverse)

model %>% compile(optimizer = optimizer_adam(lr=0.01, beta_1=0.9, beta_2=0.999, epsilon=1e-08, decay=1e-5),
                  loss = "categorical_crossentropy",
                  metrics = "accuracy")

history <- fit(model, x = train$X, y = train$Y,
               batch_size = 128, epochs = 10, shuffle = TRUE, 
               validation_data = list(val$X, val$Y))