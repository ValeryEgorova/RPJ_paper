#-------------------------------------------------------------------
# Project: RPJ paper
# Organization: SFedU Future Skills Research Lab
# Objective: Install packages needed for the analysis
# Author: Valeria Egorova
# Date: 16 June 2023
#-------------------------------------------------------------------


# List of required packages

required_packages <- c("tidyverse",
                       "haven",
                       "readxl",
                       "psych",
                       "foreign",
                       "factoextra",
                       "corrr",
                       "ggcorrplot",
                       "writexl",
                       "car",
                       "ggfortify",
                       "mgcv",
                       "erer",
                       "caret",
                       "e1071",
                       "margins",
                       "Hmisc",
                       "naniar",
                       "ggpubr",
                       "DescTools")


# Function to check and install packages
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}


# Call the function with the list of required packages
check_and_install_packages(required_packages)
