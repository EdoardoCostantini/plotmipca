#' Plot case study results
#'
#' Generate the main plot for the case study.
#'
#' @param results object containing results produced by the simulation study
#' @param y dependent variable in the substantive analysis model
#' @return Returns the ggplot
#' @author Edoardo Costantini, 2023
#' @examples
#' # Define example inputs
#' results <- dataFdd
#' y <- "PTSD-RI parent score"
#' 
#' # Use the function
#' plot_case(
#'     results = dataFdd,
#'     y = "yp"
#' )
#' 
#' @export
plot_case <- function(results, y) {

    # Plot YP
    ggplot(
        data = results,
        aes(
            x = Time,
            y = .data[[y]],
            group = rep
        )
    ) +
        facet_grid(
            rows = vars(trt),
            cols = vars(imp),
            scales = "free"
        ) +
        geom_line(linewidth = .1) +
        theme(
            # Text
            text = element_text(
                size = 10
            ),
            # Legend
            legend.position = "bottom",
            # Background
            panel.border = element_rect(fill = NA, color = "gray"),
            panel.background = element_rect(fill = NA)
        )

}