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
    ggplot2::ggplot(
        data = results,
        ggplot2::aes(
            x = Time,
            y = .data[[y]],
            group = rep
        )
    ) +
        ggplot2::facet_grid(
            rows = ggplot2::vars(trt),
            cols = ggplot2::vars(imp),
            scales = "free"
        ) +
        ggplot2::geom_line(linewidth = .1) +
        ggplot2::theme(
            # Text
            text = ggplot2::element_text(size = 10),
            # Legend
            legend.position = "bottom",
            # Background
            panel.border = ggplot2::element_rect(fill = NA, color = "gray"),
            panel.background = ggplot2::element_rect(fill = NA)
        )
}