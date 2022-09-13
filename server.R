# Project:   shiny-mi-pca-plot
# Objective: back end
# Author:    Edoardo Costantini
# Created:   2022-07-28
# Modified:  2022-09-13

server <- function(input, output, session) {

  # Width of the page
  observe({
    if (shinybrowser::get_width() < 768) {
      updateCheckboxGroupInput(session,
        inputId = "method",
        label = "Methods",
        selected = levels(gg_shape$method)[4]
      )
    }
  })

  # Plot
  output$plot <- renderPlot(res = 96, height = 500, {
    gg_shape %>%
      filter(
        j == input$j,
        par == input$par,
        lv == input$lv,
        method %in% input$method,
        npc <= input$npc[2],
        npc >= input$npc[1],
        K %in% input$K,
        pj %in% input$pj
      ) %>%
      mutate(npc = factor(npc)) %>%
      ggplot(aes_string(
        x = plot_x_axis,
        y = input$plot_y_axis,
        fill = "npc"
      )) +
      geom_bar(
        stat = "identity",
        position = "dodge"
      ) +
      scale_fill_manual(values = gray.colors(length(unique(gg_shape$npc)),
        start = .7,
        end = .1
      )) +
      facet_grid(reformulate(grid_x_axis, grid_y_axis),
        labeller = labeller(.rows = label_both, .cols = label_value),
        scales = "free",
        switch = "y"
      ) +
      theme(
        # Text
        text = element_text(size = 12),
        strip.text.y.right = element_text(angle = 0),
        # Legend
        legend.position = "right",
        # Backgorund
        panel.background = element_rect(fill = NA, color = "gray")
      ) +
      labs(
        x = plot_x_axis,
        y = input$plot_y_axis
      )
  })
}