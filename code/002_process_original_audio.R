library(caret)
library(parallel)
library(tuneR)
library(abind)

melspec <- function(x, start, end){
  wav <- readWave(filename = x) %>% 
    extractWave(from = start, to = end, xunit="time")
  # return log-spectrogram with 256 Mel bands and compression
  sp <- melfcc(wav, nbands = 256, usecmp = T,
               spec_out = T,
               hoptime = (end-start) / 256)$aspectrum
  # Median-based noise reduction
  noise <- apply(sp, 1, median)
  sp <- sweep(sp, 1, noise)
  sp[sp < 0] <- 0
  # Normalize to max
  sp <- sp / max(sp)
  return(sp)
}

# iterate melspec over all samples, arrange output into array
melslice <- function(x, from, to){
  lapply(X = x, FUN = melspec,
         start = from, end = to) %>%
    simplify2array()
}

# iterate melslice over all different time windows
audioProcess <- function(files, limit = 0.5, ws = 0.5, stride = 0.5,
                         ncores = 8){
  windowSize <- seq(0, limit, by = stride)
  # iterate and parallelise
  batches <- mclapply(windowSize, function(w){
    # execute
    melslice(files, from = w, to = w+ws)
  }, mc.cores = ncores)
  # combine output into single array
  out <- abind(batches, along = 3)
  # reorder dimensions after adding single-channel as 4th
  dim(out) <- c(dim(out), 1)
  out <- aperm(out, c(3,1,2,4))
  return(out)
}

set.seed(100)
idx <- createFolds(id, k = 10)
valIdx <- idx$Fold01
testIdx <- idx$Fold02
# Define samples for train, val and test
fnamesTrain <- fnames[-c(valIdx, testIdx)]
fnamesVal <- fnames[valIdx]
fnamesTest <- fnames[testIdx]


# Take multiple readings per sample for training
Xtrain <- audioProcess(files = fnamesTrain, ncores = 1,
                       limit = 0.5, ws = 0.5, stride = 0.5)
Xval <- audioProcess(files = fnamesVal, ncores = 1,
                     limit = 0.5, ws = 0.5, stride = 0.5)
Xtest <- audioProcess(files = fnamesTest, ncores = 1,
                      limit = 0.5, ws = 0.5, stride = 0.5)

# Define targets and augment data
target <- model.matrix(~0+id)

targetTrain <- do.call("rbind", lapply(1:(dim(Xtrain)[1]/length(fnamesTrain)),
                                       function(x) target[-c(valIdx, testIdx),]))
targetVal <- do.call("rbind", lapply(1:(dim(Xval)[1]/length(fnamesVal)),
                                     function(x) target[valIdx,]))
targetTest <- do.call("rbind", lapply(1:(dim(Xtest)[1]/length(fnamesTest)),
                                      function(x) target[testIdx,]))
# Assemble Xs and Ys
train <- list(X = Xtrain, Y = targetTrain)
val <- list(X = Xval, Y = targetVal)
test <- list(X = Xtest, Y = targetTest)
