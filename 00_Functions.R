################################################################################
#### Camtrap Class and Constructor
################################################################################
# Class containing camtrap data
setClass("camtrap"
  , slots = c(
      collectionpath  = "character"
    , collection      = "character"
    , images          = "character"
    , cameras         = "character"
    , locations       = "logical"
    , metadata        = "data.frame"
    , detections      = "data.frame"
    , classifications = "data.frame"
    , directory       = "character"
    , mainnames       = "character"
    , corrected       = "logical"
  )
  , prototype = list(directory = "NA", locations = F, corrected = F)
)

# Constructor function to createa a camtrap object
camtrap <- function(collectionpath, collection) {
  object <- new("camtrap"
    , collectionpath = collectionpath
    , collection     = collection
  )
  return(object)
}

################################################################################
#### Camtrap Methods
################################################################################
# Print method
setMethod("show", "camtrap", function(object) {
  ntot <- length(object@images)
  nmet <- nrow(object@metadata)
  ndir <- length(object@cameras)
  if(nmet > 0) {
      nmet <- paste0(nmet, "/", ntot)
    } else {
      nmet <- F
  }
  if (nrow(object@detections) > 0) {
      ndet <- sum(object@cameras %in% object@detections$Camera)
      ndet <- paste0(ndet, "/", ndir)
    } else {
      ndet <- F
  }
  class <- nrow(object@classifications) > 0
  corr <- object@corrected
  locs <- object@locations
  if (locs) {
      unlo <- sum(is.na(object@metadata$Location))
    } else {
      unlo <- ntot
  }
  cat("\n")
  cat("------------------------------------\n")
  cat("---------- camtrap object ----------\n")
  cat("------------------------------------\n")
  cat("\n")
  cat("Path to collection:\n", object@collectionpath, "\n")
  cat("\n")
  cat("Collection name:\n", object@collection, "\n")
  cat("------------------------------------\n")
  cat("Cameras                 : ", ndir, "\n")
  cat("Images                  : ", ntot, "\n")
  cat("------------------------------------\n")
  cat("Filelist loaded         : ", ntot > 0, "\n")
  cat("Metadata loaded         : ", nmet, "\n")
  cat("Detections loaded       : ", ndet, "\n")
  cat("Corrections applied     : ", corr, "\n")
  cat("Locations assigned      : ", locs, "\n")
  cat("Classifications assigned: ", class, "\n")
  cat("Unlocated images        : ", unlo, "\n")
  cat("------------------------------------\n")
  cat("camtrap object stored at: ", object@directory, "\n")
})

# Method to subset
setMethod("subset", signature(x = "camtrap"), function(x, ...) {

  # If nothing has been loaded yet, can't subset
  if (nrow(x@metadata) == 0) {
    stop("This camtrap object does not contain any data that can be subsetted. You should read in some metadata first.\n")
  }

  # Join metadata and detections so that subset can be based on information
  # from both
  if (nrow(x@detections) > 0 & nrow(x@classifications) > 0) {
      all <- left_join(x@metadata, dplyr::select(x@detections, -Camera), by = "Filepath")
      all <- left_join(all, x@classifications, by = "Filepath", relationship = "many-to-many")
    } else if (nrow(x@detections) > 0) {
      all <- left_join(x@metadata, dplyr::select(x@detections, -Camera), by = "Filepath")
    } else {
      all <- x@metadata
  }

  # Need to be careful with scopes
  condition <- substitute(...)
  indices <- eval(condition, all, parent.frame())
  all <- all[indices, ]

  # Need to make sure that all other slots are updated accordingly
  x@images   <- x@images[x@images %in% all$Filepath]
  x@metadata <- x@metadata[x@metadata$Filepath %in% all$Filepath, ]
  x@cameras  <- x@cameras[x@cameras %in% all$Camera]
  if (nrow(x@detections) > 0) {
    x@detections <- x@detections[x@detections$Filepath %in% all$Filepath, ]
  }
  if (nrow(x@classifications) > 0) {
    x@classifications <- x@classifications[x@classifications$Filepath %in% all$Filepath, ]
  }
  return(x)
})

# Method to plot
setMethod("plot", signature(x = "camtrap"), function(x, index = 1) {

  # Loop through the images and visualize them
  for (i in index) {

    # Load ith image
    path <- file.path(x@collectionpath, x@collection, x@images[i])
    img  <- image_read(path)
    dims <- image_info(img)

    # Plot base image
    img <- image_draw(img)
    text(
        0.5 * dims$width
      , 0.95 * dims$height
      , x@images[i]
      , cex = 5
      , col = "white"
      , pos = 3
    )

    # Check if there are any detections to overlay
    if (nrow(x@detections) > 0) {
        detes <- subset(x@detections, Filepath == x@images[i])
      } else {
        detes <- data.frame(Dummy = character())
    }

    # If three are, overlay them
    if (nrow(detes) > 0) {
      for (j in 1:nrow(detes)) {

        # Parse category color
        if (detes$Category[j] == "animal") {
            col <- "green"
          } else if (detes$Category[j] == "person") {
            col <- "yellow"
          } else {
            col <- "red"
        }

        # Add bounding box around the detection (color depending on detected category)
        rect(
            xleft   = detes$x_left[j] * dims$width
          , xright  = (detes$x_left[j] + detes$x_width[j]) * dims$width
          , ytop    = detes$y_top[j] * dims$height
          , ybottom = (detes$y_top[j] + detes$y_height[j]) * dims$height
          , border  = col
          , lwd     = 10
        )

        # Add a text label indicating the detected class (note that we ensure that the
        # label does not leave the image border on the y-axis).
        text(
            x      = detes$x_left[j] * dims$width
          , y      = max(detes$y_top[j], 0.05) * dims$height
          , labels = paste(detes$Category[j], detes$Confidence[j])
          , cex    = 5
          , col    = col
          , adj    = c(0, 0)
        )
      }
    }

    # Plot
    dev.off()
    plot(img)

  }
})

################################################################################
#### Processing Functions
################################################################################
# Function to check if something is installed or not
.isInstalled <- function(x) {

  # Check operating system
  unix <- !(Sys.info()["sysname"] %in% c("Windows"))
  if (unix) {
      exit_code <- suppressWarnings(system2("command", args = c("-v", x), stdout = FALSE))
    } else {
      exit_code <- system(paste("where", x), intern = TRUE)
  }
  return(exit_code == 0)
}

# Helper function to resize images
.resizeImages <- function(image_path, width = 1920, height = 1080, batchsize = 100, progress = T) {

  # Ensure that mogrify has been installed
  if (!.isInstalled("mogrify")) {
    stop("Please install ImageMagick on your system. For details see https://imagemagick.org/\n")
  }

  # Create batches
  groups       <- createGroup(image_path, batchsize = batchsize)
  ngroups      <- length(unique(groups))
  image_path_s <- split(image_path, groups)

  # Loop through the groups and apply resizing
  if (progress) {
    pb <- txtProgressBar(0, ngroups, style = 3)
  }
  for (i in 1:ngroups) {

    # Run Resizing
    command <- sprintf(paste0("mogrify -resize ", width, "x", height, " %s "), paste(image_path_s[[i]], collapse = " "))
    system(command)

    # Update progress
    if (progress) {
      setTxtProgressBar(pb, i)
    }
  }
}

# Function to rewrite images in a camtrap object
resizeImages <- function(object, width, height, outfile = NULL, overwrite = F, batchsize = 100, progress = T) {

  # Only works when metadata is not null
  if (nrow(object@metadata) == 0) {
    cat("Metadata associated with the images needs to be loaded first.\n")
  }

  # Check for images that do not fulfill the specified width and size
  # requirements
  subsetted <- subset(object, subset =
      ImageWidth < width - 1
    | ImageWidth > width + 1
    | ImageHeight < height - 1
    | ImageHeight > height + 1
  )

  # If there are no images to adjust, stop the function
  if (nrow(subsetted@metadata) == 0) {
    cat("All images already have the correct dimensions.\n")
    return(object)
  }

  # Those images need to be resized and the metadata needs to be updated. We can
  # simply subset the object accordingly and later update the metadata again
  object@metadata <- subset(object@metadata, !(Filepath %in% subsetted@images))
  images <- file.path(subsetted@collectionpath, subsetted@collection, subsetted@images)

  # Resize those images
  .resizeImages(images
    , width     = width
    , height    = height
    , batchsize = batchsize
    , progress  = progress
  )

  # Update the metadata and return the object
  object <- loadMetadata(object)
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}

# Function to store a camtrap collection
writeCamtrap <- function(object, file, overwrite = F) {
  if (file.exists(file) & !overwrite) {
    stop(file, " already exists. To overwrite, use the option 'overwrite = T'\n")
  }
  if(!file.exists(file)) {
    success <- file.create(file, showWarnings = F)
  }
  fullpath <- normalizePath(file)
  write_rds(object, fullpath)
  object@directory <- fullpath
  return(object)
}

# Function to check the validity of the file cameras
validateDirectories <- function(object) {

  # Table containing information
  cams <- tibble(Camera = object@cameras)

  # Match images to the cameras
  info <- tibble(
      Filepath = object@images
    , Camera   = sapply(strsplit(object@images, split = "/"), function(x) {x[[1]]})
    ) %>%
    nest(Images = -Camera) %>%
    left_join(cams, ., by = "Camera") %>%
    mutate(Empty = map_lgl(Images, is.null))

  # Check if any camera is lacking images
  missing <- info %>%
    subset(Empty) %>%
    pull(Camera)
  if (length(missing) > 0) {
      warning("The following folder contains no images and should be removed:", missing, ". Please reload the filelist afterwards using loadFilelist(..., force = T). \n")
    } else {
      cat("Camera folders were checked and all of them contain images.\n")
  }

  # List the fetch dates of the different cameras and check if they can be parsed correctly
  cams <- file.path(dat@collectionpath, dat@collection, object@cameras)
  noparse <- sapply(cams, function(x) {
    cams_fetchdates <- list.dirs(path = x, recursive = F, full.names = F)
    dates <- tryCatch(ymd(cams_fetchdates)
      , warning = function(w) {return(NA)}
      , error = function(e) {return(NA)}
    )
    failed <- any(is.na(dates))
    return(failed)
  })

  # Return answer
  if (any(noparse)) {
      warning("Could not parse fetchdates for:", basename(cams)[noparse], "\n")
    } else {
      cat("Successfully parsed fetchdates across all cameras.\n")
  }
}

# Function to load a camtrap collection
readCamtrap <- function(file) {
  object <- read_rds(file)
  if(inherits(object, "camtrap")) {
    object@directory <- file
    return(object)
  } else {
    stop("The provided file is not a camtrap object\n")
  }
}

# Function to identify and remove dot files
findDotfiles <- function(object, messages = T, remove = T) {
  if (length(object@images) == 0) {
    stop("Please load the filelist first.\n")
  }
  files <- dir(
      path         = file.path(dat@collectionpath, dat@collection)
    , include.dirs = F
    , full.names   = T
    , all.files    = T
    , recursive    = T
    , pattern      = "\\._|\\.DS_Store"
  )
  if (!remove) {
    return(files)
  }
  file.remove(files)
}

# Function to update the collectionpath
updateCollectionpath <- function(object, collectionpath) {
  object@collectionpath <- collectionpath
  return(object)
}

# Function to check if a collection exists
collectionExists <- function(object) {
  dir.exists(file.path(object@collectionpath, object@collection))
}

# Function to create group variable based on size of dataframe and batchsize
createGroup <- function(x, batchsize = 1) {
  if (is.data.frame(x)) {
    n <- dim(x)[1]
  } else if (is.vector(x)) {
    n <- length(x)
  }
  ngroups <- n %/% batchsize + 1
  group <- sort(rep_len(1:ngroups, n))
  return(group)
}

# Function to load filelist
loadFilelist <- function(object, force = F, outfile = NULL, progress = T, overwrite = F) {


  # Check if the collection exists
  path <- file.path(object@collectionpath, object@collection)
  if (!collectionExists(object)) {
    stop("Collection ", path, " does not exist\n")
  }

  # Check if filelist has already been loaded
  if (length(object@images) != 0 & !force) {
    cat("List has already been loaded and will not be loaded again. If you want to enforce loading it again, use the option 'force = T'\n")
    return(object)
  } else {

    # Throw warning if file should be saved but already exists and overwrite = F
    if (!is.null(outfile)) {
      if (file.exists(outfile) & !overwrite) {
        stop(file, " already exists. To overwrite, use the option 'overwrite = T'")
      }
    }

    dirs <- list.dirs(
        path       = path
      , recursive  = F
      , full.names = F
    )
    if (progress) {
      cat("Loading file list...\n")
      pb <- txtProgressBar(min = 0, max = length(dirs), style = 3)
    }
    images <- sapply(1:length(dirs), function(x) {
      file_list <- dir(
          path         = file.path(path, dirs[x])
        , include.dirs = F
        , full.names   = F
        , recursive    = T
        , pattern      = ".JPG$"
      )
      if (length(file_list) > 0) {
        file_list <- paste0(dirs[x], "/", file_list)
      }
      if (progress) {
        setTxtProgressBar(pb, x)
      }
      return(file_list)
    })
    images <- unlist(images)

    # If metadata is already present, update it
    if (nrow(object@metadata) > 0) {
      object@metadata <- object@metadata[object@metadata$Filepath %in% object@images, ]
    }

    # Update object
    object@images <- images
    object@cameras <- dirs
    if (!is.null(outfile)) {
      object <- writeCamtrap(object, outfile, overwrite = overwrite)
    }
    return(object)
  }
}

# Function to load metadata (in batches)
loadMetadata <- function(object, batchsize = 1000, force = F, outfile = NULL, progress = T, overwrite = F) {

  # Check if the collection exists
  path <- file.path(object@collectionpath, object@collection)
  if (!collectionExists(object)) {
    stop("Collection ", path, " does not exist.\n")
  }

  # Check if the filelist has been loaded
  if (length(object@images) == 0) {
    stop("Filelist has not been loaded yet. Run 'loadFiles()' first.\n")
  }

  # Total number of images
  path <- object@images
  n <- length(path)

  # Metadata for some images may already exist. Maybe some images were removed
  # in the meantime. We'll have to remove their metadata too.
  if (force) {
    object@metadata <- data.frame()
  }
  if (nrow(object@metadata) > 0) {
    object@metadata <- object@metadata[object@metadata$Filepath %in% path, ]
    path <- path[!(path %in% object@metadata$Filepath)]
  }

  # Stop if there is no more data that needs to be loaded
  if (length(path) == 0) {
    cat("All metadata has already been loaded.\n")
    return(object)
  }

  # Throw warning if file should be saved but already exists and overwrite = F
  if (!is.null(outfile)) {
    if (file.exists(outfile) & !overwrite) {
      stop(file, " already exists. To overwrite, use the option 'overwrite = T'")
    }
  }

  # If any additional metadata is loaded, the locations need to be reloaded as
  # well
  if (object@locations) {
    cat("New metadata is being loaded. You will need to assign the locations again.\n")
    object@locations <- F
    object@metadata  <- dplyr::select(object@metadata, all_of(object@mainnames))
  }

  # Create overview table of images that still need to be loaded
  info <- tibble(
      Filepath  = path
    , Filetype  = substr(Filepath, start = nchar(Filepath) - 2, stop = nchar(Filepath))
    , Camera    = sapply(strsplit(path, split = "/"), function(x) {x[[1]]})
    , FetchDate = sapply(strsplit(path, split = "/"), function(x) {x[[2]]}) %>% ymd()
    , Group     = createGroup(Filepath, batchsize = batchsize)
  )

  # Go through the groups, read their metadat, and assign to existing
  # metadata
  if (progress) {
    cat("Loading metadata. This may take a while.\n")
    pb <- txtProgressBar(min = 0, max = n, style = 3)
  }
  for (i in unique(info$Group)) {
    subinfo <- subset(info, Group == i)
    newmeta <- read_exif(
        file.path(object@collectionpath, object@collection, subinfo$Filepath)
      , tags = c( "CreateDate", "ImageWidth", "ImageHeight")
    )
    newmeta$CreateDate <- ymd_hms(newmeta$CreateDate)
    newmeta$SourceFile <- NULL
    newmeta            <- cbind(subinfo, newmeta)
    newmeta$Group      <- NULL
    object@metadata    <- rbind(object@metadata, newmeta)
    if (!is.null(outfile)) {
      object <- writeCamtrap(object, outfile, overwrite = overwrite)
    }
    if (progress) {
      setTxtProgressBar(pb, nrow(object@metadata))
    }
  }
  object@mainnames <- names(object@metadata)
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}

# Function to update metadata
updateMetadata <- function(object, outfile = NULL, progress = T, overwrite = F) {

  # Throw warning if file should be saved but already exists and overwrite = F
  if (!is.null(outfile)) {
    if (file.exists(outfile) & !overwrite) {
      stop(file, " already exists. To overwrite, use the option 'overwrite = T'")
    }
  }

  # Create a new filelist
  cat("Updating filelist\n")
  object <- loadFilelist(object, force = T, progress = progress)

  # Check discrepancy between filelist and metadata
  keep <- object@metadata$Filepath %in% object@images
  object@metadata <- object@metadata[keep, ]
  toadd <- object@images[!(object@images %in% object@metadata$Filepath)]
  if (length(toadd) > 0) {
    object <- loadMetadata(object, progress = progress)
  }

  # Return the updated object
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}

# Function to list all json files in a collection
findJson <- function(object) {
  dirs <- object@cameras
  files <- sapply(1:length(dirs), function(x) {
    file_list <- dir(
        path         = file.path(object@collectionpath, object@collection, dirs[x])
      , include.dirs = F
      , full.names   = F
      , recursive    = T
      , pattern      = ".json$"
    )
    file_list <- file.path(dirs[x], file_list)
    return(file_list)
  })
  files <- unlist(files)

  # Remove any checkpoints
  indices <- grepl(files, pattern = "checkpoint")
  files <- files[!indices]

  # Put all into a dataframe
  files <- tibble(Filepath = files)

  # Parse the camera identifier
  info <- separate(files, col = Filepath, sep = "\\/", into = c("Camera", "Filename"))
  info <- bind_cols(files, info)

  # Return result
  return(info)
}

# Function to remove json checkpoints
removeJsonCheckpoint <- function(object) {
  dirs <- list.dirs(file.path(object@collectionpath, object@collection), recursive  = F, full.names = F)
  files <- sapply(1:length(dirs), function(x) {
    file_list <- dir(
        path         = file.path(object@collectionpath, object@collection, dirs[x])
      , include.dirs = F
      , full.names   = T
      , recursive    = T
      , pattern      = ".json$"
    )
    return(file_list)
  })
  files <- unlist(files)

  # Remove any checkpoints
  indices <- grepl(files, pattern = "checkpoint")
  checkpoints <- files[indices]
  file.remove(checkpoints)
}

# Function to read the megadetector json file nicely
parseJson <- function(file) {

  # Read json and exract categories
  anot               <- fromJSON(file = file)
  cats               <- enframe(anot$detection_categories)
  names(cats)        <- c("CategoryID", "CategoryName")
  cats$CategoryName  <- unlist(cats$CategoryName)

  # Get information into nice format
  info <- tibble(
        Filepath     = map_chr(anot$images, "file")
      , Detections = lapply(anot$images, function(x){
        tibble(
            Category   = map_chr(x$detections, "category")
          , Confidence = map_dbl(x$detections, "conf")
          , bbox       = map(x$detections, "bbox")
        )
      })
    ) %>%
    unnest(Detections) %>%
    mutate(bbox = map(bbox, function(x){paste0(x, collapse = ", ")})) %>%
    separate(bbox
      , sep     = ","
      , into    = c("x_left", "y_top", "x_width", "y_height")
      , convert = T
    )

  # Join categories to the info table
  info$Category <- cats$CategoryName[match(info$Category, cats$CategoryID)]

  # Return all
  return(info)

}

# Function to load detections
loadDetections <- function(object, outfile = NULL, progress = T, force = F, overwrite = F) {

  # If force = T, remove existing detections
  if (force) {
    object@detections <- data.frame()
  }

  # Find all json files that have not been loaded yet
  if (nrow(object@detections) > 0) {
      allcam <- object@cameras
      loaded <- unique(object@detections$Camera)
      toload <- allcam[!(allcam %in% loaded)]
      if (length(toload) == 0) {
        cat("All detections have already been loaded. Use option 'force = T' to enforce reloading them.\n")
        return(object)
      }

      # Create a dummy "camtrap" object subsetting to the cameras for which
      # detections still need to be loaded. Then search for detection files.
      dummy_object <- subset(object, Camera %in% toload)
      json <- findJson(dummy_object)
    } else {
      json <- findJson(object)
  }

  # If there are no additional detections, say so
  if (nrow(json) == 0) {
    cat("There are no (additional) detections that can be loaded.\n")
    return(object)
  }

  # Throw warning if file should be saved but already exists and overwrite = F
  if (!is.null(outfile)) {
    if (file.exists(outfile) & !overwrite) {
      stop(file, " already exists. To overwrite, use the option 'overwrite = T'")
    }
  }

  # If there are files to be loaded, run through them and load them
  files <- file.path(object@collectionpath, object@collection, json$Filepath)
  if (progress) {
    cat("Reading detections. This may take a while.\n")
    pb <- txtProgressBar(0, length(files), style = 3)
  }
  json$Detections <- lapply(1:length(files), function(x) {
    dets <- parseJson(files[x])
    if (progress) {
      setTxtProgressBar(pb, x)
    }
    return(dets)
  })
  json$Filepath <- NULL
  json$Filename <- NULL
  json          <- unnest(json, Detections)
  json$Filepath <- file.path(json$Camera, json$Filepath)

  # Put everything into the camtrap object
  object@detections <- rbind(object@detections, json)
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}

# Function to download megadetector files
downloadMegadetector <- function(directory, timeout = 600) {

  # Create megadetector directory
  directory <- file.path(directory, "Megadetector")

  # Check if the directory already exists
  if (!dir.exists(directory)) {
    create <- readline(prompt = paste0(directory, " does not exist. Should it be created? y/n: "))
    if (create %in% c("Y", "y")) {
      dir.create(directory)
    } else {
      stop("Please provide a valid directory.\n")
    }
    cat(directory, "was successfully created.\n")
  }

  # Adjust timeout
  timeout_orig <- getOption("timeout")
  options(timeout = timeout)

  # Download files from megadetector if necessary
  files <- c("yolov5", "cameratraps", "ai4eutils")
  exist <- file.exists(file.path(directory, files))
  files <- files[!exist]
  for (i in files) {
    if (i == "yolov5") {
        url = "https://github.com/ecologize/yolov5/archive/refs/heads/master.zip"
      } else if (i == "cameratraps") {
        url = "https://github.com/ecologize/CameraTraps/archive/refs/heads/master.zip"
      } else {
        url = "https://github.com/Microsoft/ai4eutils/archive/refs/heads/master.zip"
    }

    # Download files
    download.file(
        url      = url
      , destfile = file.path(directory, paste0(i, ".zip"))
    )

    # unzip the .zip file
    zipname <- basename(unzip(file.path(directory, paste0(i, ".zip")), list = T)$Name[1])
    unzip(file.path(directory, paste0(i, ".zip")), exdir = directory)
    file.rename(from = file.path(directory, zipname), to = file.path(directory, i))
    file.remove(file.path(directory, paste0(i, ".zip")))

  }

  # Download the latest models
  if (!file.exists(file.path(directory, "md_v5a.0.0.pt"))) {
    download.file(
        url      = "https://github.com/microsoft/CameraTraps/releases/download/v5.0/md_v5a.0.0.pt"
      , destfile = file.path(directory, "md_v5a.0.0.pt")
    )
  }
  if (!file.exists(file.path(directory, "md_v5b.0.0.pt"))) {
    download.file(
        url      = "https://github.com/microsoft/CameraTraps/releases/download/v5.0/md_v5b.0.0.pt"
      , destfile = file.path(directory, "md_v5b.0.0.pt")
    )
  }

  # We also need to install the bashscript to initiate the megadetector
  download.file(
      url = "https://raw.githubusercontent.com/DavidDHofmann/RunMegaDetector/main/RunMegaDetector.sh"
    , destfile = file.path(directory, "RunMegaDetector.sh")
  )

  # Reset the timeout option
  options(timeout = timeout_orig)

  # Return message
  cat("All files have been successfully downloaded.\n")
}

# Function to setup the megadetector environment
installMegadetector <- function(megadetector_dir, environment = "detector") {

  # Ensure the provided environment is valid
  environment <- match.arg(environment, choices = c(
      "detector"
    , "detector-m1"
    , "detector-mac"
    , "detector-mac-nomkl"
    , "detector-opencv-headless"
    , "unpinned"
  ))

  # Ensure that mamba is installed
  if (!.isInstalled("mamba")) {
    stop("Please install mamba. Then restart the session.")
  }

  # Check if the cameratraps environment already exists, otherwise, install it
  envs <- conda_list()
  if (!("cameratraps-detector" %in% envs$name)) {
    envspecs <- file.path(megadetector_dir, paste0("cameratraps/envs/environment-", environment, ".yml"))
    command <- paste0("mamba env create --file ", envspecs)
    system(command)
  }

  # Return success message
  cat("Megadetector environment was successfully installed.\n")
}

# Function to check if the megadetector works as anticipated
checkMegadetector <- function(megadetector_dir, error = T) {

  # Verify that required folders and files are existing
  direcs         <- file.path(megadetector_dir, c("yolov5", "cameratraps", "ai4eutils"))
  models         <- file.path(megadetector_dir, c("md_v5a.0.0.pt", "md_v5b.0.0.pt"))
  runfile        <- file.path(megadetector_dir, "RunMegaDetector.sh")
  dirs_exist     <- dir.exists(direcs)
  models_exist   <- file.exists(models)
  runfile_exists <- file.exists(runfile)

  # Check if the megadetector environment is installed
  env_exists <- "cameratraps-detector" %in% conda_list()$name

  # If anything is missing, state this
  if (!all(c(dirs_exist, models_exist, runfile_exists, env_exists))) {
    if (error) {
      stop("Some files are missing. Please run 'downloadMegadetector()'\n")
    } else {
      cat("Some files are missing. Please run 'downloadMegadetector()'\n")
    }
  } else if (!env_exists) {
    if (error) {
      stop("cameratraps-detector environment does not exist. Please run 'installMegadetector()'\n")
    } else {
      cat("cameratraps-detector environment does not exist. Please run 'installMegadetector()'\n")
    }
  }
}

# Function to run the megadetector
runMegadetector <- function(object
    , megadetector_dir
    , python_dir
    , model
    , checkpoint_freq = 10000
    , outfile
    , messages = T
    , overwrite = F
  ) {

  # Throw warning if file should be saved but already exists and overwrite = F
  if (!is.null(outfile)) {
    if (file.exists(outfile) & !overwrite) {
      stop(file, " already exists. To overwrite, use the option 'overwrite = T'")
    }
  }

  # Create vector of detection files that should exist
  detfiles <- file.path(object@collectionpath, object@collection, object@cameras, "Detections.json")

  # Check which files already exist
  if (all(file.exists(detfiles))) {
    cat("Detection files for all cameras already exist. Megadetector will not be run.\n")
    return(object)
  }

  # Verify a correct model is selected
  if (length(model) > 1) {
    stop("'model' should be of length 1.\n")
  }
  model      <- match.arg(model, choices = c("5a", "5b"))
  model_file <- ifelse(model == "5a", yes = "md_v5a.0.0.pt", no = "md_v5b.0.0.pt")

  # Ensure the images exist
  if (!collectionExists(object)) {
    stop("Collection ", object@collection, " does not exist\n")
  }

  # Build the command to run the megadetector
  part_01 <- "bash"
  part_02 <- file.path(megadetector_dir, "RunMegaDetector.sh")
  part_03 <- shQuote(file.path(object@collectionpath, object@collection))
  part_04 <- megadetector_dir
  part_05 <- model_file
  part_06 <- python_dir
  part_07 <- checkpoint_freq
  command <- paste(part_01, part_02, part_03, part_04, part_05, part_06, part_07)

  # Run the command
  system(command, ignore.stdout = !messages, ignore.stderr = !messages)

  # Once it's done, load all detections and assign them to the object
  object <- loadDetections(object)
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}

# Function to parse the deployments excel sheet
parseDeployments <- function(x) {

  # Load deployments so we can assign locations to each image
  deploy <- read_xlsx(x
    , col_types = c("text", "text", "numeric", "numeric", "date", "date", "date", "date", "text", "numeric", "numeric", "numeric", "guess", "text", "numeric", "text", "text")
    , na        = "NA"
  )

  # Do some cleaning of the times and dates
  deploy <- deploy %>%
    mutate(
        StartDate = ymd(StartDate)
      , EndDate   = ymd(EndDate)
      , StartTime = as_hms(StartTime)
      , EndTime   = as_hms(EndTime)
    ) %>%
    mutate(
      , Start     = ymd_hms(paste(StartDate, StartTime))
      , End       = suppressWarnings(ymd_hms(paste(EndDate, EndTime)))
    ) %>%
    mutate(
      End = if_else(is.na(End), now(tzone = "UTC"), End)
    ) %>%
    dplyr::select(-c(StartTime, EndTime, StartDate, EndDate)) %>%
    dplyr::select(Camera = CameraID, Start, End, Longitude, Latitude, everything())

}

# Function to parse corrections excel sheet
parseCorrections <- function(x) {
  corrections <- x %>%
    read_xlsx() %>%
    subset(!is.na(`Correction (days)`))
  return(corrections)
}

# Function to parse classifications from trap tagger
parseClassifications <- function(path) {
  df <- read_csv(path)
  names(df) <- c("Filepath", "Species")
  df$Filepath <- sapply(df$Filepath, function(x) {
    paste0(strsplit(x, "/")[[1]][-c(1, 2)], collapse = "/")
  })
  return(df)
}

# Function to assign classifications from trap tagger
assignClassifications <- function(object, classifications, outfile = NULL) {
  classifications_sub <- subset(classifications, Filepath %in% object@images)
  object@classifications <- classifications_sub
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}

# Function to apply corrections
applyCorrections <- function(object, corrections, outfile = NULL, progress = T, overwrite = F) {

  # Throw warning if file should be saved but already exists and overwrite = F
  if (!is.null(outfile)) {
    if (file.exists(outfile) & !overwrite) {
      stop(file, " already exists. To overwrite, use the option 'overwrite = T'")
    }
  }

  # Ensure that corrections haven't been applied already
  if (object@corrected) {
    stop("Corrections have already been applied. If you want to reapply updated corrections, you'll need to re-read the metadata\n")
  }

  # Extract current metadata
  meta <- object@metadata

  # Only apply corrections to the current collection
  corrrect_collec <- subset(corrections, Collection == object@collection)

  # If there are no corrections to go through, return the object
  if (nrow(corrrect_collec) == 0) {
    cat("There are no corrections for collection:", object@collection, "\n")
    return(object)
  }

  # Indicate that corrections were applied
  object@corrected <- T

  # Apply the corrections to the metadata
  if (progress) {
    pb <- txtProgressBar(0, nrow(corrrect_collec), style = 3)
  }
  corrected <- list()
  for (i in 1:nrow(corrrect_collec)) {
    indices <- which(meta$Camera == corrrect_collec$Camera[i] &
      meta$CreateDate >= corrrect_collec$From[i] &
      meta$CreateDate <= corrrect_collec$To[i]
    )
    if (length(indices) == 0) {
        if (progress) {
          setTxtProgressBar(pb, i)
        }
        next
      } else {
        meta_tocorrect <- meta[indices, ]
        meta_tocorrect$CreateDate <- meta_tocorrect$CreateDate +
          ddays(corrrect_collec$`Correction (days)`[i])
        corrected[[i]] <- meta_tocorrect
        meta <- meta[-indices, ]
        if (progress) {
          setTxtProgressBar(pb, i)
        }
    }
  }

  # Bind the corrected data to the data that was already correct
  meta <- corrected %>%
    do.call(rbind, .) %>%
    rbind(., meta) %>%
    arrange(Camera, FetchDate, CreateDate)

  # Add it to the object
  object@metadata <- meta
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}


# Function to assign locations to the images This function also allows to
# cluster images that were collected at nearby stations.
assignLocations <- function(object, deployments, outfile = NULL, cluster = 0, force = F, overwrite = F) {

  # Throw warning if file should be saved but already exists and overwrite = F
  if (!is.null(outfile)) {
    if (file.exists(outfile) & !overwrite) {
      stop(file, " already exists. To overwrite, use the option 'overwrite = T'")
    }
  }

  # Check if locations have already been loaded
  if (object@locations & !force) {
    cat("Locations have already been loaded and will not be loaded again. If you want to enforce loading them again, use the option 'force = T'\n")
    return(object)
  }
  if (object@locations & force) {
    object@metadata <- dplyr::select(object@metadata, all_of(object@mainnames))
  }

  # Take the deployments and create unique locations
  if (cluster > 0) {
      locations <- deployments %>%
        dplyr::select(Longitude, Latitude) %>%
        as.matrix() %>%
        vect(., crs = "epsg:4326") %>%
        distance() %>%
        hclust() %>%
        cutree(h = cluster) %>%
        cbind(deployments, Location = .)
    } else {
      locations <- deployments %>%
        dplyr::select(Longitude, Latitude) %>%
        distinct() %>%
        mutate(Location = 1:n()) %>%
        left_join(deployments, ., by = c("Longitude", "Latitude"))
  }

  # Let's create a unique identifier for each location
  locations <- mutate(locations, Location = paste0("L", sprintf("%03d", Location)))
  locations <- as_tibble(locations)

  # We also want to assign the deployment information to each image. Thus, loop
  # through the deployments and assign the associated images
  locations$Images <- pbmclapply(
      X                  = 1:nrow(locations)
    , ignore.interactive = T
    , mc.cores           = detectCores() - 1
    , FUN                = function(x) {
    imgs <- subset(object@metadata
      , Camera == locations$Camera[x] &
        CreateDate >= locations$Start[x] &
        CreateDate <= locations$End[x]
    )
    imgs$Camera <- NULL
    return(imgs)
  })
  locations <- unnest(locations, Images)
  locations <- dplyr::select(locations, all_of(object@mainnames), everything())

  # Identify images that are still missing a location
  missing <- object@metadata[!(object@metadata$Filepath %in% locations$Filepath), ]

  # Bind this together
  metadata <- bind_rows(locations, missing)

  # Update camtrap object and return it
  #   object@unlocated <- any(is.na(metadata$Longitude))
  object@metadata  <- metadata
  object@locations <- T
  if (!is.null(outfile)) {
    object <- writeCamtrap(object, outfile, overwrite = overwrite)
  }
  return(object)
}

# Function to extract data from a camtrap object
extractData <- function(object, what = "metadata") {
  what <- match.arg(what, choices = c("metadata", "unlocated", "detections", "locations"))
  if (what == "metadata") {
    toreturn <- left_join(object@metadata, dplyr::select(object@detections, -Camera), by = "Filepath", keep = )
  } else if (what == "unlocated") {
    toreturn <- subset(object@metadata, is.na(Location))
  } else if (what == "detections") {
    toreturn <- object@detections
  } else {
    toreturn <- object@metadata %>%
      dplyr::select(c(Location, Longitude, Latitude)) %>%
      distinct()
  }
  return(toreturn)
}

# Function to get a summary of the unlocated images
summarizeUnlocated <- function(object) {

  # Ensure that locations are assigned but that unlocated images exist
  if (!object@locations) {
      stop("You haven't assigned locations to the cameras yet. Run 'asssingLocations() first.'\n")
    } else if (sum(is.na(object@metadata$Location)) == 0) {
      stop("There are no unlocated images in this collection.\n")
  }

  # Create summary
  summarised <- object@metadata %>%
    subset(is.na(Location)) %>%
    nest(Images = -c(Camera)) %>%
    mutate(map_df(Images, function(x) {
      data.frame(From = min(x$CreateDate), To = max(x$CreateDate))
    }))

  # Return it
    return(summarised)
}

# Function to copy images to locations
transferImages <- function(object, directory, collectionname = object@collection, overwrite = F, batchsize = 1000, progress = T, unlocated = F) {

  # Prepare names
  collectionpath <- directory
  collection     <- collectionname
  file           <- paste0(file.path(directory, collection), ".rds")

  # Ensure that the provided directory is not the same as the collection directory
  if (collectionpath == object@collectionpath & collection == object@collection) {
    stop("Provided collection name and output directry are the same as in the current collection. Please change the output directory or assign a new collectionnname\n")
  }

  # Ensure the output file and directory does not already exist
  if (file.exists(file) & !overwrite) {
    stop(file, " already exists. To overwrite, use the option 'overwrite = T'\n")
  }
  if (dir.exists(file.path(collectionpath, collection)) & !overwrite) {
    stop(file.path(collectionpath, collection), " already exists. Please remove it if you want to copy data gain.\n")
  }

  # Copy images with locations
  if (!unlocated) {

    # Ensure that locations are assigned to the object
    if (!object@locations) {
      stop("Images have no locations assigned yet. Run 'assignLocations()' first\n")
    }

    # Subset
    object <- subset(object, !is.na(Location))

  }

  # If all files are unlocated and the user still wants to copy files, we just
  # use the camera as the location
  if (!object@locations & unlocated) {
    object@metadata$Location <- object@metadata$Camera
  }

  # Define new filepaths
  paths <- dplyr::select(object@metadata, Filepath, Camera, Location)
  paths <- distinct(paths)
  if (!object@locations) {
      paths$NewFilepath <- file.path(paths$Filepath)
    } else {
      paths$NewFilepath <- file.path(paths$Location, paths$Filepath)
  }
  paths$OldFilepathFull <- file.path(object@collectionpath, object@collection, paths$Filepath)
  paths$NewFilepathFull <- file.path(collectionpath, collection, paths$NewFilepath)

  # Create directories that do not exist yet
  dirs <- unique(dirname(paths$NewFilepathFull))
  dirs <- dirs[!dir.exists(dirs)]
  if (length(dirs) > 0) {
    for (i in dirs) {
      dir.create(i, recursive = T, showWarnings = F)
    }
  }

  # Copy the files in batches
  paths$Group <- createGroup(1:nrow(paths), batchsize = batchsize)
  ngroups      <- length(unique(paths$Group))
  if (progress) {
    cat("Copying images to the output directory.\n")
    pb <- txtProgressBar(0, ngroups, style = 3)
  }
  for (i in 1:ngroups) {
    paths_group_i <- subset(paths, Group == i)
    file.copy(paths_group_i$OldFilepathFull, paths_group_i$NewFilepathFull, overwrite = overwrite)
    if (progress) {
      setTxtProgressBar(pb, i)
    }
  }

  # We also want to keep track of the location coordinates
  if (object@locations) {
    location_file <- file.path(collectionpath, paste0(collection, "_Locations.csv"))
    write_csv(extractData(object, "locations"), location_file)
  }

  # Finally, let's also copy the camtrap object, subsetted to exactly
  # those images. Note that we'll need to update all filepaths to reflect that
  # they are now organized by locations
  object_updated                <- object
  object_updated@collectionpath <- collectionpath
  object_updated@collection     <- collection

  # Update filepaths
  object_updated@metadata$Filepath   <- paths$NewFilepath[match(object_updated@metadata$Filepath, paths$Filepath)]
  object_updated@images              <- paths$NewFilepath[match(object_updated@images, paths$Filepath)]
  object_updated@detections$Filepath <- paths$NewFilepath[match(object_updated@detections$Filepath, paths$Filepath)]

  # Store it
  writeCamtrap(object_updated, file, overwrite = overwrite)

}
