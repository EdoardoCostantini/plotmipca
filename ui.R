# Project:   shiny-mi-pca-plot
# Objective: user interface
# Author:    Edoardo Costantini
# Created:   2022-07-28
# Modified:  2022-08-01

# Packages ---------------------------------------------------------------------

  library(shiny)
  library(ggplot2)
  library(shinyWidgets)
  library(dplyr)
  library(shinybrowser) # web browser information in Shiny apps

  # Preapre data -----------------------------------------------------------------

  # Data to plot
  gg_shape <- readRDS("8469421_main_gg_shape.rds")

  # Change names of factors for plot
  levels(gg_shape$method) <- c("MI-PCR-ALL", "MI-PCR-ALL (oracle)",
                               "MI-PCR-AUX", "MI-PCR-VBV",
                               "MI-QP", "MI-OR", "MI-MI",
                               "CC", "OG")

  gg_shape$npc <- as.numeric(as.character(gg_shape$npc))

  # Graph structure
  plot_x_axis <- "K"
  moderator   <- "npc"
  grid_x_axis <- "method"
  grid_y_axis <- "pj"

# Ui

ui <- fluidPage(
  fluidRow(
    column(4,
           hr(),
           h4("Data generation"),
           radioButtons("lv",
                        "Latent structure",
                        choices = rev(unique(gg_shape$lv)),
                        selected = TRUE,
                        inline = TRUE),
           checkboxGroupInput("K",
                              "Discrete levels",
                              inline = TRUE,
                              choices = levels(gg_shape$K),
                              selected = levels(gg_shape$K)[c(1, 3, 5)]),
           checkboxGroupInput("pj",
                              "Proportion of noise variables",
                              inline = TRUE,
                              choices = unique(gg_shape$pj),
                              selected = unique(gg_shape$pj)[c(1, 4)]),
    ),
    column(4,
           hr(),
           h4("Outcome measires"),
           selectInput("par",
                       "Parameter",
                       choices = levels(gg_shape$par)),
           radioButtons("plot_y_axis",
                       "Outcome measure",
                       choices = c("bias", "CIC", "CIW", "mcsd"),
                        inline = TRUE),
    ),
    column(4,
           hr(),
           h4("Missing data treatments"),
           checkboxGroupInput("method",
                              "Methods",
                              choices = levels(gg_shape$method),
                              selected = levels(gg_shape$method)[c(1, 3:5)],
                              inline = TRUE),
           shinyWidgets::sliderTextInput(inputId = "npc",
                                         label = "Number of principal components",
                                         hide_min_max = TRUE,
                                         choices = sort(unique(gg_shape$npc)),
                                         selected = range(gg_shape$npc),
                                         grid = TRUE),
    )
  ),

  # Silent extraction of size
  shinybrowser::detect(),

  hr(),

  plotOutput('plot'),
)
