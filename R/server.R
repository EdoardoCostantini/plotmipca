#' server
#'
#' server function for the shiny app
#'
#' @param input set of inputs coming from ui
#' @param output set of outputs
#' @param session session info and status
#' @author Edoardo Costantini, 2023
#' @export
server <- function(input, output, session) {
    # Width of the page
    observe({
        if (shinybrowser::get_width() < 768) {
            updateCheckboxGroupInput(session,
                inputId = "method",
                label = "Methods",
                selected = levels(dataResults$method)[4]
            )
        }
    })

    # Simulation study plot ------------------------------------------------

    output$plot <- renderPlot(res = 96, height = 500, {
        dataResults %>%
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
                x = "K",
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
            scale_fill_manual(
                values = c(gray.colors(2, start = 0.5, end = 0.8), "white")
            ) +
            facet_grid(reformulate("method", "pj"),
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
                x = "K",
                y = input$plot_y_axis
            )
    })

    # Simulation study traceplots ------------------------------------------

    output$mids_sim_plot <- renderPlot(
        res = 96,
        height = 750,
        {
            trace_plot(
                mids_data = dataMids$sim,
                method = input$conv_sim_method,
                var = input$conv_sim_var,
                iters = input$conv_case_iters
            )
        }
    )

    # > Case study traceplots ----------------------------------------------

    output$mids_case_plot <- renderPlot(
        res = 96,
        height = 750,
        {
            trace_plot(
                mids_data = dataMids$fdd,
                method = input$conv_case_method,
                var = input$conv_case_var,
                iters = input$conv_sim_iters
            )
        }
    )
}
