# Project:   shiny-mi-pca-plot
# Objective: pre-process input data for actual use in shiny app
# Author:    Edoardo Costantini
# Created:   2022-09-13
# Modified:  2022-10-25

# library(shiny)
# library(ggplot2)
# library(shinyWidgets)
# library(dplyr)
# library(shinybrowser) # web browser information in Shiny apps

# Prepare data -----------------------------------------------------------------

# Data to plot
ggshapeld <- readRDS("./data/9950505_main_gg_shape.rds") # 1st BRM submission w/ correct MI-OP
ggshapehd <- readRDS("./data/9987321_main_gg_shape.rds") # HD version with full pj

# Combine data
ggshape <- rbind(
  cbind(j = 56, ggshapeld),
  cbind(j = 242, ggshapehd)
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
methods_names <- methods_names[methods_names$original %in% levels(ggshape$method), ]
levels(ggshape$method) <- methods_names$plot

# Round pj
ggshape$pj <- round(ggshape$pj, 2)

# Make a different factor for labelling npcs
ggshape$NPC <- ggshape[, "npc"]
levels(ggshape$NPC) <- list(
  "7+" = as.character(7:max(as.numeric(levels(ggshape[, "npc"])))),
  "1 to 6" = as.character(1:6),
  "0" = c("0")
)

# Make npc names better
ggshape$npc <- as.numeric(as.character(ggshape$npc))

# Make Parameter names better
current_levels <- levels(ggshape$par)

current_levels <- gsub("~1", " mean", current_levels)
current_levels <- gsub("r", " correlation ", current_levels)
current_levels <- gsub("~~", " covariance ", current_levels)
levels(ggshape$par) <- current_levels

# Set data for use
usethis::use_data(ggshape, overwrite = TRUE)

# Graph structure
plot_x_axis <- "K"
moderator <- "npc"
grid_x_axis <- "method"
grid_y_axis <- "pj"
