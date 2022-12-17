# Project:   plot.mi.pca
# Objective: Function to run app
# Author:    Edoardo Costantini
# Created:   2022-12-02
# Modified:  2022-12-02
# Notes:

#' plotResults
#'
#' Starts a Shiny app to interact with the results of the \href{https://github.com/EdoardoCostantini/mi-pca}{mi-pca} project.
#' 
#' @export
#' @import shiny 
#' @import shinybrowser
#' @import dplyr
#' @import ggplot2
#' @import shinyWidgets
#' @import pkgload
#' @return Shiny app UI.
#' 
plotResults <- function() {

    # Set up -------------------------------------------------------------------
    
    # Graph structure
    plot_x_axis <- "K"
    moderator <- "npc"
    grid_x_axis <- "method"
    grid_y_axis <- "pj"

    
    # UI -----------------------------------------------------------------------

    ui <- fluidPage(
        fluidRow(
            column(
                4,
                hr(),
                h4("Data generation"),
                radioButtons("j",
                    "Number of observed items",
                    choices = unique(ggshape$j),
                    selected = unique(ggshape$j)[1],
                    inline = TRUE
                ),
                radioButtons("lv",
                    "Latent structure",
                    choices = rev(unique(ggshape$lv)),
                    selected = TRUE,
                    inline = TRUE
                ),
                checkboxGroupInput("K",
                    "Discrete levels",
                    inline = TRUE,
                    choices = levels(ggshape$K),
                    selected = levels(ggshape$K)[c(1, 3, 5)]
                ),
                checkboxGroupInput("pj",
                    "Proportion of noise variables",
                    inline = TRUE,
                    choices = unique(ggshape$pj),
                    selected = unique(ggshape$pj)[c(1, 4)]
                ),
            ),
            column(
                4,
                hr(),
                h4("Outcome measures"),
                selectInput("par",
                    "Parameter",
                    choices = levels(ggshape$par),
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
                    choices = levels(ggshape$method),
                    selected = levels(ggshape$method)[c(1, 3:5)],
                    inline = TRUE
                ),
                shinyWidgets::sliderTextInput(
                    inputId = "npc",
                    label = "Number of principal components (NPC)",
                    hide_min_max = TRUE,
                    choices = sort(unique(ggshape$npc)),
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

    # Server -------------------------------------------------------------------

    server <- function(input, output, session) {
        # Width of the page
        observe({
            if (shinybrowser::get_width() < 768) {
                updateCheckboxGroupInput(session,
                    inputId = "method",
                    label = "Methods",
                    selected = levels(ggshape$method)[4]
                )
            }
        })

        # Plot
        output$plot <- renderPlot(res = 96, height = 500, {
            ggshape %>%
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
                    group = "npc",
                    fill = "NPC"
                )) +
                geom_bar(
                    stat = "identity",
                    position = "dodge",
                    colour = "black",
                    size = .25
                ) +
                scale_fill_manuaRl(
                    values = c(gray.colors(2, start = 0.5, end = 0.8), "white")
                ) +
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

    # Run app ------------------------------------------------------------------

    shinyApp(ui, server, ...)
}