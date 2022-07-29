# Project:   shiny-mi-pca-plot
# Objective: back end
# Author:    Edoardo Costantini
# Created:   2022-07-28
# Modified:  2022-07-28

  # Packages ---------------------------------------------------------------------

  library(shiny)
  library(ggplot2)
  library(shinyWidgets)
  library(dplyr)

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

server <- function(input, output, session) {

  output$plot <- renderPlot(res = 96, height = 1000, {
    gg_shape %>%
      filter(par == input$par,
             lv == input$lv,
             method %in% input$method,
             npc <= input$npc[2],
             npc >= input$npc[1],
             K %in% input$K,
             pj %in% input$pj) %>%
      mutate(npc = factor(npc))  %>%
      ggplot(aes_string(x = plot_x_axis,
                        y = input$plot_y_axis,
                        fill = "npc")) +
      geom_bar(stat = "identity",
               position = "dodge") +
      scale_fill_manual(values = gray.colors(length(unique(gg_shape$npc)),
                                             start = .7,
                                             end = .1)) +
      # geom_hline(aes(yintercept = 10),
      #            size = .25) +
      facet_grid(reformulate(grid_x_axis, grid_y_axis),
                 labeller = labeller(.rows = label_both, .cols = label_value),
                 scales = "free",
                 switch = "y") +
      theme(
        # Text
        text = element_text(size = 12),
        strip.text.y.right = element_text(angle = 0),
        # Legend
        legend.position = "right",
        # Backgorund
        panel.background = element_rect(fill = NA, color = "gray")
      ) +
      labs(x = plot_x_axis,
           y = input$plot_y_axis)
  })

}