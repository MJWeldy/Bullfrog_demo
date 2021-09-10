# multiple implementations of a convolutional neural network for
# classifying bullfrog calls from audio data

source("./code/001_load_original_data.R") ## 998 audio files
# audio processing with a base-R workflow
source("./code/002_process_original_audio.R") # 1996 audio files: 2 0.5s clips from the front of each .wav file
  # Visualize a spectrogram
  image(train$X[1,,,],
        xlab = "Time (s)",
        ylab = "Frequency (kHz)",
        axes = F)
  # Generate mel sequence from Hz points, standardize to plot
  freqs <- c(0, 1, 5, 15, 22.05)
  mels <- 2595 * log10(1 + (freqs*1e3) / 700) # https://en.wikipedia.org/wiki/Mel_scale
  mels <- mels - min(mels)
  mels <- mels / max(mels)  
  
  axis(1, at = seq(0, 1, by = .2), labels = seq(0, 0.5, by = 0.1))
  axis(2, at = mels, las = 2,
       labels = round(freqs, 2))
  
#### Save ####
#save(train, val, test, file = "prepAudio.RData")
#load("prepAudio.RData")

source("./code/003_CNN_model_structure.R")
summary(model)
source("./code/004_CNN_train_original_data.R")
plot(history)
source("./code/005_evaluate_CNN_original_data.R")
head(predProb) # class prediction probabilities
confMat 
mean(predClass == trueClass)  # Accuracy 0.96


# audio processing with a tidyverse
source("./code/006_create_half_second_clips.R")
source("./code/007_process_half_second_clips.R") #3916 audio clips: each audio file 
#cut into 0.5s non-overlapping clips
source("./code/008_CNN_train_half_second_clips.R")
plot(history)
source("./code/009_evaluate_CNN_half_second_clips.R")
head(predProb) # class prediction probabilities
confMat 
mean(predClass == trueClass)  # Accuracy 0.96