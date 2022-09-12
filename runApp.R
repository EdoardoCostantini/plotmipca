# Project:   shiny-mi-pca-plot
# Objective: script to load data and run app
# Author:    Edoardo Costantini
# Created:   2022-07-28
# Modified:  2022-09-12

# Packages ---------------------------------------------------------------------

  library(shiny)
  library(ggplot2)
  library(shinyWidgets)
  library(dplyr)
  library(shinybrowser) # web browser information in Shiny apps

# Preapre data -----------------------------------------------------------------

  # Data to plot
  gg_shape <- readRDS("9950505_main_gg_shape.rds")

  # Change names of factors for plot
  levels(gg_shape$method) <- c("MI-PCR-ALL", "MI-PCR-ALL (oracle)",
                               "MI-PCR-AUX", "MI-PCR-VBV",
                               "MI-QP", "MI-OR", "MI-MI",
                               "CC", "OG")

  gg_shape$npc <- as.numeric(as.character(gg_shape$npc))

  # Graph structure
  plot_x_axis <- "K"
  moderator <- "npc"
  grid_x_axis <- "method"
  grid_y_axis <- "pj"

# Run app ----------------------------------------------------------------------

  runApp()

# Send to server ---------------------------------------------------------------

  rsconnect::deployApp()