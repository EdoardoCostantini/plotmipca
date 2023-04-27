# Project:   plotmipca
# Objective: Function to run app
# Author:    Edoardo Costantini
# Created:   2022-12-02
# Modified:  2022-12-17
# Notes:

#' plotResults
#'
#' Starts a Shiny app to interact with the results of the \href{https://github.com/EdoardoCostantini/mi-pca}{mi-pca} project.
#'
#' @details
#' The interface of the Shiny app allows you to change the values of the following simulation study experimental factors:
#'
#' - Number of observed items
#' - Presence of latent structure (if yes, 7 latent variables are used)
#' - Discrete levels (number of categories for discretized items)
#' - Proportion of noise variables
#' - Missing data treatments:
#'
#'      - MI-PCR-ALL: mi-pca using pca on all data, after a round of single imputation
#'      - MI-PCR-ALL (oracle): mi-pca using pca on all data computed on the originally fully observed data
#'      - MI-PCR-AUX: mi-pca using pca only on auxiliary variables
#'      - MI-PCR-VBV: mi-pca using pca only on a variable-by-variable basis
#'      - MI-QP: mice using the quickpred function from the R package mice to select the predictors for the univariate imputation models via the correlation-based threshold
#'      - MI-OR: mice with oracle knowledge on which predictors to use (the univariate imputation models included the other variables under imputation and the predictors that were used to impose missingness)
#'      - MI-MI: mice with minimal missing data models (only variables under imputation used as predictors in the imputation models)
#'      - CC: complete case analysis
#'      - OG: original fully-observed data
#'
#' - Number of principal components
#' - Outcome measure:
#'
#'      - (percent relative) bias
#'      - CIC (confidence interval coverage)
#'      - CIW (average confidence interval)
#'      - mcsd (standard deviation of the estimate across the monte carlo simulations)
#'
#' - Parameter (which statistic for which variable)
#'
#' @export
#' @import shiny
#' @import shinybrowser
#' @import dplyr
#' @import ggplot2
#' @import shinyWidgets
#' @import pkgload
#' @import mice
#' @import lattice
#' @return Shiny app UI.
#'
plotResults <- function() {
    # Set up -------------------------------------------------------------------

    # Graph structure
    plot_x_axis <- "K"
    moderator <- "npc"
    grid_x_axis <- "method"
    grid_y_axis <- "pj"

    # Server -------------------------------------------------------------------

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
                scale_fill_manual(
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

        # Simulation study traceplots ------------------------------------------

        output$mids_sim_plot <- renderPlot(
            res = 96,
            height = 750,
            {
                cnd_id <- grep(input$conv_sim_method, names(dataMids$sim))

                # Work with simple object name
                x <- dataMids$sim[[cnd_id]]

                # Default arguments that you could change in MICE
                type <- "l"
                col <- 1:10
                lty <- 1
                theme <- mice::mice.theme()
                layout <- c(2, 1)

                # Extract objects I need
                mn <- x$chainMean
                sm <- sqrt(x$chainVar)
                m <- x$m
                it <- x$iteration

                # select subset of non-missing entries
                # obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
                # varlist <- names(obs)[obs]
                varlist <- input$conv_sim_var
                # varlist <- "z1"

                # Prepare objects for plotting
                mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
                sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
                adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
                data <- cbind(adm, rbind(mn, sm))
                colnames(data) <- c(".it", ".m", ".ms", varlist)

                # Create formula
                formula <- as.formula(paste0(
                    paste0(varlist, collapse = "+"),
                    "~.it|.ms"
                ))

                # Dummy to trick R CMD check
                .m <- NULL
                rm(.m)

                # Load function to obtain the correct plot arrangement
                strip.combined <- function(which.given, which.panel, factor.levels, ...) {
                    if (which.given == 1) {
                        lattice::panel.rect(0, 0, 1, 1,
                            col = theme$strip.background$col, border = 1
                        )
                        lattice::panel.text(
                            x = 0, y = 0.5, pos = 4,
                            lab = factor.levels[which.panel[which.given]]
                        )
                    }
                    if (which.given == 2) {
                        lattice::panel.text(
                            x = 1, y = 0.5, pos = 2,
                            lab = factor.levels[which.panel[which.given]]
                        )
                    }
                }

                # Make plot
                lattice::xyplot(
                    x = formula, data = data, groups = .m,
                    type = type, lty = lty, col = col, layout = layout,
                    scales = list(
                        y = list(relation = "free"),
                        x = list(alternating = FALSE)
                    ),
                    as.table = TRUE,
                    xlim = c(input$conv_sim_iters[1] - 1, input$conv_sim_iters[2] + 1),
                    xlab = "Iteration",
                    ylab = "",
                    strip = strip.combined,
                    par.strip.text = list(lines = 0.5),
                    aspect = 9 / 16
                )
            }
        )

        # > Case study traceplots ----------------------------------------------

        output$mids_case_plot <- renderPlot(
            res = 96,
            height = 750,
            {
                cnd_id <- grep(input$conv_case_method, names(dataMids$fdd))

                # Work with simple object name
                x <- dataMids$fdd[[cnd_id]]

                # Default arguments that you could change in MICE
                type <- "l"
                col <- 1:10
                lty <- 1
                theme <- mice::mice.theme()
                layout <- c(2, 1)

                # Extract objects I need
                mn <- x$chainMean
                sm <- sqrt(x$chainVar)
                m <- x$m
                it <- x$iteration

                # select subset of non-missing entries
                # obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
                # varlist <- names(obs)[obs]
                varlist <- input$conv_case_var
                # varlist <- "z1"

                # Prepare objects for plotting
                mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
                sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
                adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
                data <- cbind(adm, rbind(mn, sm))
                colnames(data) <- c(".it", ".m", ".ms", varlist)

                # Create formula
                formula <- as.formula(paste0(
                    paste0(varlist, collapse = "+"),
                    "~.it|.ms"
                ))

                # Dummy to trick R CMD check
                .m <- NULL
                rm(.m)

                # Load function to obtain the correct plot arrangement
                strip.combined <- function(which.given, which.panel, factor.levels, ...) {
                    if (which.given == 1) {
                        lattice::panel.rect(0, 0, 1, 1,
                            col = theme$strip.background$col, border = 1
                        )
                        lattice::panel.text(
                            x = 0, y = 0.5, pos = 4,
                            lab = factor.levels[which.panel[which.given]]
                        )
                    }
                    if (which.given == 2) {
                        lattice::panel.text(
                            x = 1, y = 0.5, pos = 2,
                            lab = factor.levels[which.panel[which.given]]
                        )
                    }
                }

                # Make plot
                lattice::xyplot(
                    x = formula, data = data, groups = .m,
                    type = type, lty = lty, col = col, layout = layout,
                    scales = list(
                        y = list(relation = "free"),
                        x = list(alternating = FALSE)
                    ),
                    as.table = TRUE,
                    xlim = c(input$conv_case_iters[1] - 1, input$conv_case_iters[2] + 1),
                    xlab = "Iteration",
                    ylab = "",
                    strip = strip.combined,
                    par.strip.text = list(lines = 0.5),
                    aspect = 9 / 16
                )
            }
        )
    }

    # Run app ------------------------------------------------------------------

    shinyApp(
        ui = ui_call(),
        server
    )
}
