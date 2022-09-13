# Project:   shiny-mi-pca-plot
# Objective: initialization script
# Author:    Edoardo Costantini
# Created:   2022-09-13
# Modified:  2022-09-13

# Packages ---------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(shinybrowser) # web browser information in Shiny apps

# Preapre data -----------------------------------------------------------------

# Data to plot
gg_shape_ld <- readRDS("9950505_main_gg_shape.rds") # 1st BRM submission w/ correct MI-OP
gg_shape_hd <- readRDS("9987321_main_gg_shape.rds") # HD version with full pj

# Combine data
gg_shape <- rbind(
  cbind(j = 56, gg_shape_ld),
  cbind(j = 242, gg_shape_hd)
)

# Map names of methods
methods_names <- data.frame(
  original = c(
    "all", "all_oracle", "aux", "vbv",
    "MIOP", "MIOR", "MIMI", "CC", "OG"
  ),
  plot = c(
    "MI-PCR-ALL", "MI-PCR-ALL (oracle)", "MI-PCR-AUX", "MI-PCR-VBV",
    "MI-QP", "MI-OR", "MI-MI", "CC", "OG"
  )
)

# Change names of factors for plot
methods_names <- methods_names[methods_names$original %in% levels(gg_shape$method), ]
levels(gg_shape$method) <- methods_names$plot

# Round pj
gg_shape$pj <- round(gg_shape$pj, 2)

# Make npc names better
gg_shape$npc <- as.numeric(as.character(gg_shape$npc))

# Graph structure
plot_x_axis <- "K"
moderator <- "npc"
grid_x_axis <- "method"
grid_y_axis <- "pj"
