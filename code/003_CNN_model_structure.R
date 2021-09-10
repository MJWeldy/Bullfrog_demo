library(keras)
k_backend()
library(tidyverse)

# Model Structure
model <- keras_model_sequential() %>% 
  layer_conv_2d(input_shape = dim(train$X)[2:4], 
                filters = 16, kernel_size = c(3, 3),
                activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = .2) %>% 
  
  layer_conv_2d(filters = 32, kernel_size = c(3, 3),
                activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = .2) %>% 
  
  layer_conv_2d(filters = 64, kernel_size = c(3, 3),
                activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = .2) %>% 
  
  layer_conv_2d(filters = 128, kernel_size = c(3, 3),
                activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(26, 2)) %>%
  layer_dropout(rate = .2) %>%
  
  layer_flatten() %>% 
  
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(rate = .5) %>% 
  layer_dense(units = ncol(train$Y), activation = "softmax")
