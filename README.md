# camtrap
This repository provides a few functions that enable you to pre-process
cameratrap data in `R` an object-oriented manner. At this stage, I merely
provide the functions, but in the future hope to put together a proper
R-package that bundles and documents them neatly and installs all dependencies
for you. For now, you can utilize the functionalities through the following two
R-scripts:

- '00_Functions.R' contains all main functions that are needed for processing
  cameratrap data. You can make them available to your current R-session using
  the command `source("00_Functions.R")`.
- '01_Processing.R' is simply an example script that showcases how to apply the
  different functions. It guides you through a typical processing workflow.
  You'll note that you need to define several paths in the preamble:
  - `camtrap` is the path to the `00_Functions.R` file.
  - `megadir` is the directory that contains all megadetector folders and
    files. Please follow my [installation
    guide](https://daviddhofmann.github.io/post/2021-04-15-megadetector/) of
    the megadetector for this.
  - `pythondir` is the directory where your python installation lives. If you
    use conda, this should be `something/miniconda3`.
  - `imagedir` is the hard drive where your cameratrap images are stored.
    (organized by camera)
  - `transfer` is the hard drive where you'd like to transfer the processed
    images to.
  - `collection` is merely the name of the image collection you'd like to process
  - `deployment` is the excel sheet containing deployment information of your cameras.
  - `correction` is the excel sheet containing correction information of your cameras.

Currently, things will probably not work out of the box, as you'll need to have
the `megadetector` and `ImageMagick` installed. In addition, there are rather
strict assumptions about how you store your data
(`Hard-Drive/Collection/Camera/FetchDate/.../Image.JPEG`) and the columns of
the different excel files. This should be generalized in the future. However,
feel free to reach out to me, and I will try to get you set up.
