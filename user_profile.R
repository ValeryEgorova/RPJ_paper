#-------------------------------------------------------------------
# Project: RPJ paper
# Organization: SFedU Future Skills Research Lab
# Author: Valeria Egorova
# Date: 16 June 2024
#-------------------------------------------------------------------


# set working directories and all directories 
# this is the profile for the RPJ paper
# this profile should be loaded before running any other script

# for Windows
USERNAME    <- Sys.getenv("USERNAME")
# for Mac
USER        <- Sys.getenv("USER")

#version from everyone, the profile works for everyone

if (USERNAME == "Админ") {
  projectFolder  <- getwd()
} 

if (USERNAME == "Valery") {
  projectFolder  <- getwd()
} 

if (USER == "Админ") {
  projectFolder  <- getwd()
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))

# set up key folders
documentation <-  file.path(projectFolder, "00_documentation")
inputData     <-  file.path(projectFolder, "01_input_data")
rcodes        <-  file.path(projectFolder, "02_codes")
outData       <-  file.path(projectFolder, "03_outputs/0301_data")
outTables     <-  file.path(projectFolder, "03_outputs/0302_tables")  
outFigures    <-  file.path(projectFolder, "03_outputs/0303_figures")


stopifnot(dir.exists(documentation))
stopifnot(dir.exists(inputData))
stopifnot(dir.exists(rcodes))
stopifnot(dir.exists(outData))
stopifnot(dir.exists(outTables))
stopifnot(dir.exists(outFigures))

