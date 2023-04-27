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

    output$plot <- renderPlot(
        res = 96,
        height = 500,
        {
            plot_simulation(
                results = dataResults,
                n_items = input$j,
                parameter = input$par,
                latent_structure = input$lv,
                method_vector = input$method,
                npc_range = input$npc,
                categories = input$K,
                prop_noise = input$pj,
                outcome = input$plot_y_axis
            )
        }
    )

    # Simulation study traceplots ------------------------------------------

    output$mids_sim_plot <- renderPlot(
        res = 96,
        height = 750,
        {
            trace_plot(
                mids_data = dataMids$sim,
                method = input$conv_sim_method,
                var = input$conv_sim_var,
                iters = input$conv_sim_iters
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
                iters = input$conv_case_iters
            )
        }
    )
}
