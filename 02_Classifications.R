################################################################################
#### Linking Back Trap Tagger Classifications
################################################################################
# Clear R's brain
rm(list = ls())

################################################################################
#### Paths and Packages
################################################################################
# Specify necessary directories (DO NOT POINT TO THE RAW DATA HERE!!!)
camtrap    <- "/home/david/ownCloud/01_Private/Bibliothek/Wissen/R-Scripts/camtrap"
imagedir   <- "/media/david/CAMERA_MAST/UZH_CameratrapSurvey/Data/02_Processed"
collection <- "Collection_2022-10"

# Specify file to which you want to store the camtrap object
file       <- file.path(imagedir, paste0(collection, ".rds"))
file_final <- file.path(imagedir, paste0(collection, "_Final.rds"))

# Load custom functions
source(file.path(camtrap, "00_Functions.R"))

# Change the working directory
library(exifr)       # To read image metadata
library(tidyverse)   # For data wrangling
library(lubridate)   # To work with dates
library(hms)         # To work with times
library(rjson)       # To read json files
library(readxl)      # To read excel files
library(magick)      # To visualize images
library(pbmcapply)   # To run stuff in parallel

# Load preprocessed data
dat <- readCamtrap(file)
dat <- updateCollectionpath(dat, imagedir)

# Check if a classification exists
classi <- paste0(file.path(dat@collectionpath, dat@collection), "_Classifications.csv")
if (file.exists(classi)) {
  classi <- parseClassifications(classi)
  dat_assigned <- assignClassifications(dat, classi)
  writeCamtrap(dat_assigned, file_final)
}

# Can visualize some classifications if we want!
par(mfrow = c(3, 2))
subi <- subset(dat_assigned, Species == "Caracal")
plot(subi, index = sample(nrow(subi@metadata), size = 6))

library(terra)
library(mapview)
test <- subi@metadata
test <- vect(test, geom = c("Longitude", "Latitude"), crs = "epsg:4326")
test <- as(test, "Spatial")
mapview(test)
