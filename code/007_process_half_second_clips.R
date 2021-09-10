library(readr)
library(tidyverse)
library(tuneR)
library(seewave)
library(abind)

half_second_tags <- read_csv("audio/half_second_tags.csv")
IDs <- ifelse(half_second_tags$`MANUAL ID`=="Background",0,1)%>% factor()

window_length = 0.5
step = 0.5

av_func <- function(x) {
  tmp <- readWave(filename = x)
  length(tmp@left)/tmp@samp.rate
}
test_df <- data.frame(fnames=fnames, id=id) %>% 
  mutate(duration = map_dbl(fnames, av_func),
         n_periods = floor(duration/window_length)
  )

fft_win <- function(x, f_name) {
  window_length = 0.5
  wav <- readWave(filename = f_name) %>% 
    extractWave(from = x, to = (x + window_length), xunit="time") %>% 
    rmnoise(f = 48000, output = "Wave")
  sp <- melfcc(wav,
               sr=48000,
               wintime = 0.025,
               hoptime = window_length / 256,
               nbands = 256, 
               usecmp = T,
               spec_out = T,
               minfreq=0,
               maxfreq=10000,
  )$aspectrum
  sp <- sp / max(sp)
  sp <- sp %>% simplify2array()
  return(sp)
}

df_with_arrays <- test_df %>%
  pmap_dfr(function(...) {
    current <- tibble(...)
    windowseq <- seq(from = 0, to = current$n_periods*0.5, by = 0.5)
    windowstart <- windowseq[1:length(windowseq)-1]  
    
    current <- current %>% 
      mutate(array_list = list(map(windowstart, fft_win, f_name = current$fnames)))
  })
df_with_arrays <- unnest(df_with_arrays, cols = c(array_list))
matrix_storage <- abind(df_with_arrays$array_list, along=3)
dim(matrix_storage) <- c(dim(matrix_storage), 1)
matrix_storage <- aperm(matrix_storage, c(3,1,2,4))


set.seed(100)
idx <- createFolds(IDs, k = 10)
trainIdx <- unlist(idx[3:10])
valIdx <- idx$Fold01
testIdx <- idx$Fold02

train <- list(X = matrix_storage[trainIdx,,,1], Y= to_categorical(IDs[trainIdx]))
dim(train$X) <- c(dim(train$X), 1)
val <- list(X = matrix_storage[valIdx,,,1], Y= to_categorical(IDs[valIdx]))
dim(val$X) <- c(dim(val$X), 1)
test <- list(X = matrix_storage[testIdx,,,1], Y= to_categorical(IDs[testIdx]))
dim(test$X) <- c(dim(test$X), 1)
# apply(train$Y,2, sum)
# apply(val$Y,2, sum)
# apply(test$Y,2, sum)