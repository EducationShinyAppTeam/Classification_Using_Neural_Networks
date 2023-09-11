irisTrain <- iris %>%
  group_by(
    Species
  ) %>%
  slice_sample(
    prop = 0.8
  )

library(dplyr)
library(tuneR)
library(tidyverse)
library(neuralnet)

setwd("C:/Users/chapp/Documents/BOAST Program 2023/Classification_Using_Neural_Networks")

trainNNData <- read.csv(file = "trainNNData.csv", header = TRUE)
trainNNData <- na.omit(trainNNData)
binaryTrainData <- trainNNData %>%
  filter(digit == 0 | digit == 1)

audio_data <- list()
mlfcc_data_list <- list()

preprocess_audio <- function(file_path, target_size) {
  audio <- readWave(file_path)
  
  # Convert audio to spectrogram
  spec <- spectrogram(audio, window = hanning(512), overlap = 256)
  
  # Convert spectrogram to image
  spec_image <- abs(spec$spec)
  
  # Resize the image to the target size
  resized_image <- imager::resize(spec_image, target_size)
  
  # Normalize pixel values to [0, 1]
  normalized_image <- resized_image / max(resized_image)
  
  return(normalized_image)
}

# Find the maximum number of frames in mlfcc_data_list
max_frames <- max(sapply(mlfcc_data_list, nrow))

for (i in 1:nrow(binaryTrainData)) {
  file_path <- paste("www/", binaryTrainData$filedir[i], sep="")
  audio_data[[i]] <- readWave(file_path)
  
  mlfcc_data_list[[i]] <- melfcc(audio_data[[i]], numcep = 13, sr = audio_data[[i]]@samp.rate)
  
  
  current_frames <- nrow(mlfcc_data_list[[i]])
  if (current_frames < max_frames) {
    mlfcc_data_list[[i]] <- rbind(mlfcc_data_list[[i]], matrix(0, nrow = max_frames - current_frames, ncol = ncol(mlfcc_data_list[[i]])))
  } else if (current_frames > max_frames) {
    mlfcc_data_list[[i]] <- mlfcc_data_list[[i]][1:max_frames, ]
  }
}


input_data <- do.call(rbind, mlfcc_data_list)


input_data <- scale(input_data)


binaryTrainData$digit <- as.numeric(binaryTrainData$digit)


formula <- as.formula(paste("digit ~ ."))


training_data <- cbind(as.data.frame(input_data), digit = binaryTrainData$digit)
model <- neuralnet(formula, data = training_data, hidden = c(10, 5))




save(irisTrain, model1, file = "~/Example.RData")

load("~/Example.RData")
