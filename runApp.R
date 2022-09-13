# Project:   shiny-mi-pca-plot
# Objective: script to load data and run app
# Author:    Edoardo Costantini
# Created:   2022-07-28
# Modified:  2022-09-13

# Load environment -------------------------------------------------------------

  source("init.R")

# Run app ----------------------------------------------------------------------

  runApp()

# Send to server ---------------------------------------------------------------

  rsconnect::deployApp()