library(tidyverse)
library(tuneR)

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

# create directory to hold the audio
ifelse(dir.exists("./audio/wav_output"),"Move along, directory exists",dir.create("./audio/wav_output"))

#function to write and rename audio files
fft_win <- function(x, f_name) {
  wav <- readWave(filename = f_name) %>%
    extractWave(from = x, to = (x + window_length), xunit="time")
  path <- str_split(f_name, pattern = "/")
  tmp <- path[[1]][[4]]
  tmp <- str_split(tmp, pattern = ".wav")[[1]][[1]]
  tmp <- paste0(path[[1]][[3]], "_",tmp,"_",x,".wav")
  write_path <- paste0("./audio/wav_output/",tmp)
  writeWave(wav, write_path, extensible = TRUE)
  return(write_path)
}

df_with_arrays <- test_df %>%
  pmap_dfr(function(...) {
    current <- tibble(...)
    windowseq <- seq(from = 0, to = current$n_periods*0.5, by = 0.5)
    windowstart <- windowseq[1:length(windowseq)-1]  
    current <- current %>% 
      mutate(array_list = list(map(windowstart, fft_win, f_name = current$fnames)))
  })

# Create csv with data for tagging the 0.5s clips in Kaleidoscope
kaleidoscope_df <- data_frame(fnames=fnames
) %>% 
  mutate(duration = map_dbl(fnames, av_func),
         n_periods = floor(duration/window_length),
         folder = map_chr(fnames, function(x) paste(str_split(x,"/")[[1]][2:3],collapse="/")),
         file = map_chr(fnames, function(x) str_split(x,"/")[[1]][4])
  )
win_func <- function(x) {
  tmp <- seq(0, x, by = step)
  tmp <-  seq(0, x, by = step)[-length(tmp)]
  return(tmp)
}
kaleidoscope_df$OFFSET <- map(kaleidoscope_df$duration, win_func)
kaleidoscope_df <- unnest(kaleidoscope_df)
write.csv(kaleidoscope_df,"kaleidoscope_prep.csv")