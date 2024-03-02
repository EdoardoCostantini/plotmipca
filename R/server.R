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
                selected = levels(plotmipca::dataResults$method)[4]
            )
        }
    })

    # Simulation study plot ------------------------------------------------

    output$plot <- renderPlot(
        res = 96,
        height = 725,
        {
            plot_simulation(
                results = plotmipca::dataResults,
                n_items = input$j,
                parameter = input$par,
                latent_structure = input$lv,
                method_vector = input$method,
                npc_range = input$npc,
                categories = input$K,
                prop_noise = input$pn,
                outcome = input$plot_y_axis
            )
        }
    )

    # Simulation study traceplots ------------------------------------------

    output$mids_sim_plot <- renderPlot(
        res = 96,
        height = 725,
        {
            plot_trace(
                mids_data = plotmipca::mids_sim,
                method = input$conv_sim_method,
                layout = c(2, 4),
                iters = input$conv_sim_iters
            )
        }
    )

    # > Case study: results ----------------------------------------------------
    output$case_plot_res <- renderPlot(
        res = 96,
        height = 725,
        {
            plot_case(
                results = plotmipca::dataFdd,
                y = input$res_case_dv
            )
        }
    )

    # > Case study traceplots ----------------------------------------------

    output$mids_case_plot <- renderPlot(
        res = 96,
        height = 725,
        {
            plot_trace(
                mids_data = plotmipca::mids_case,
                method = input$conv_case_method,
                layout = c(2, 6),
                iters = input$conv_case_iters
            )
        }
    )
}
