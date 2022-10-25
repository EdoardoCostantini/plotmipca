# Project:   shiny-mi-pca-plot
# Objective: user interface
# Author:    Edoardo Costantini
# Created:   2022-07-28
# Modified:  2022-10-25

# Load environment
source("init.R")

# Define UI
ui <- fluidPage(
  fluidRow(
    column(
      4,
      hr(),
      h4("Data generation"),
      radioButtons("j",
        "Number of observed items",
        choices = unique(gg_shape$j),
        selected = unique(gg_shape$j)[1],
        inline = TRUE
      ),
      radioButtons("lv",
        "Latent structure",
        choices = rev(unique(gg_shape$lv)),
        selected = TRUE,
        inline = TRUE
      ),
      checkboxGroupInput("K",
        "Discrete levels",
        inline = TRUE,
        choices = levels(gg_shape$K),
        selected = levels(gg_shape$K)[c(1, 3, 5)]
      ),
      checkboxGroupInput("pj",
        "Proportion of noise variables",
        inline = TRUE,
        choices = unique(gg_shape$pj),
        selected = unique(gg_shape$pj)[c(1, 4)]
      ),
    ),
    column(
      4,
      hr(),
      h4("Outcome measures"),
      selectInput("par",
        "Parameter",
        choices = levels(gg_shape$par),
        selected = "z1 correlation z2"
      ),
      radioButtons("plot_y_axis",
        "Outcome measure",
        choices = c("bias", "CIC", "CIW", "mcsd"),
        inline = TRUE
      ),
    ),
    column(
      4,
      hr(),
      h4("Missing data treatments"),
      checkboxGroupInput("method",
        "Methods",
        choices = levels(gg_shape$method),
        selected = levels(gg_shape$method)[c(1, 3:5)],
        inline = TRUE
      ),
      shinyWidgets::sliderTextInput(
        inputId = "npc",
        label = "Number of principal components (NPC)",
        hide_min_max = TRUE,
        choices = sort(unique(gg_shape$npc)),
        selected = c(0, 10),
        grid = TRUE
      ),
    )
  ),

  # Silent extraction of size
  shinybrowser::detect(),
  hr(),
  plotOutput("plot"),
)
