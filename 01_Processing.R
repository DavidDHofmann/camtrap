################################################################################
#### Preprocessing the Cameratrap Data
################################################################################
# Clear R's brain
rm(list = ls())

################################################################################
#### Paths and Packages
################################################################################
# Specify necessary directories
camtrap    <- "/home/david/ownCloud/Dokumente/Bibliothek/Wissen/R-Scripts/camtrap"
megadir    <- "/home/david/Megadetector"
imagedir   <- "/media/david/CAMERA_BACK"
transfer   <- "/media/david/CAMERA_BACK/Processed"
collection <- "Collection_2024-06"

# Only necessary on mac and linux
pythondir  <- "/home/david/miniconda3"

# # Specify necessary directories
# camtrap    <- "C:/Users/david/switchdrive/Dokumente/Bibliothek/Wissen/R-Scripts/camtrap"
# megadir    <- "C:/Megadetector"
# imagedir   <- "C:/Users/david/Desktop"
# transfer   <- "C:/Users/david/Desktop"
# collection <- "SampleImages"
# 
# # Only necessary on mac and linux
# pythondir  <- "/home/david/miniconda3"

# deployment <- "/home/david/ownCloud/University/15. PhD/General/Cameratrapping/01_General/01_Deployments.xlsx"
# correction <- "/home/david/ownCloud/University/15. PhD/General/Cameratrapping/01_General/04_Corrections.xlsx"

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
library(terra)       # To do spatial data
library(pbmcapply)   # To run stuff in parallel
library(reticulate)  # To use python functions

################################################################################
#### Verify Installation
################################################################################
# Check if all is installed
checkMegadetector(megadir)

# If not installed already, download and install it
# downloadMegadetector(megadir)
# installMegadetector(megadir, environment = "detector")

################################################################################
#### Pre-Requisites
################################################################################
# Load deployments, corrections, and classifications (if they exist)
# deploy <- parseDeployments(deployment) %>% select(Camera, Longitude, Latitude, Start, End)
# correc <- parseCorrections(correction)

# Specify filepath to which the camtrap object should be stored
if (file.exists(file)) {
    cat(file, "already exists and will be loaded\n")
    dat <- readCamtrap(file)
  } else {
    cat(file, "does not exist and will be created\n")
    dat <- camtrap(collectionpath = imagedir, collection = collection)
    if (collectionExists(dat)) {
        dat <- writeCamtrap(dat, file)
      } else {
        stop("Specified collection does not exist.")
    }
}

################################################################################
#### Processing
################################################################################
# Update collectionpath
dat <- updateCollectionpath(dat, imagedir)

# Show it's contents
show(dat)

# Make sure it exists
collectionExists(dat)

# Read the filelist
dat <- loadFilelist(dat, outfile = file, overwrite = T)
show(dat)

# Remove dot-files
# findDotfiles(dat, remove = F)
# findDotfiles(dat, remove = T)

# Verify the file directories
validateDirectories(dat)

# Read the metadata
dat <- loadMetadata(dat, outfile = file, batchsize = 1000, overwrite = T)

# Check image dimensions
table(dat@metadata$ImageWidth)
table(dat@metadata$ImageHeight)

# Resize images that require resizing
# dat <- resizeImages(dat
#   , width     = 1920
#   , height    = 1080
#   , outfile   = file
#   , overwrite = T
# )

# Read detections
# dat <- loadDetections(dat, outfile = file, overwrite = T)
# dat

# Write to file
writeCamtrap(dat, file = file, overwrite = T)

# Run the detector
dat <- runMegadetector(dat
  , megadetector_dir = megadir
  , python_dir       = pythondir
  , model            = "5a"
  , checkpoint_freq  = 1000
  , outfile          = file
  , messages         = T
  , overwrite        = T
)

# Remove any megadetector checkpoints
# removeJsonCheckpoint(dat)

# Apply corrections
# dat <- readCamtrap(file_final)
# dat <- applyCorrections(dat, correc, outfile = file_final)

# Add clustered locations (set cluster to the desired distance in meters)
# dat <- assignLocations(dat, deployments = deploy, outfile = file_final, cluster = 500)
# show(dat)

# Are there any unlocated images? If so, give details
# nrow(extractData(dat, "unlocated"))
# summarizeUnlocated(dat)

# Get images (with animals)
# subdat <- subset(dat, Category == "animal" & Confidence >= 0.1)
# subdat <- subset(dat, Category == "animal")
# plot(subdat, index = 34)

# If you'd like to drop images containing humans
# toremove <- subset(dat, Category == "person")
# subdat <- subset(subdat, !(Filepath %in% toremove@images))

################################################################################
#### Transfer Images
################################################################################
# Transfer them to the output hard-drive
# show(subdat)
# transferImages(subdat
#   , directory      = transfer
#   , collectionname = paste0(collection, "_Animals_NoHumans")
# #   , collectionname = collection
#   , batchsize      = 1000
#   , progress       = T
#   , unlocated      = T
#   , overwrite      = T
# )
#
# ################################################################################
# #### Link Back Classifications
# ################################################################################
# # Load the collection
# dat <- readCamtrap("/home/david/Schreibtisch/Example/HardDrive2/Collection_2023-05.rds")
#
# # Load classifications
# classi <- parseClassifications("/home/david/Schreibtisch/Classifications.csv")
#
# # Assign them
# dat <- assignClassifications(dat, classi)
#
# ################################################################################
# #### Visualizations
# ################################################################################
# # Visualize some detections
# subdat <- subset(dat, Category %in% c("person", "vehicle", "animal") & Confidence > 0.1)
# par(mfrow = c(2, 2))
# plot(subdat, index = sample(nrow(subdat@metadata), size = 4))
