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
                    choices = unique(dataResults$j),
                    selected = unique(dataResults$j)[1],
                    inline = TRUE
                ),
                radioButtons("lv",
                    "Latent structure",
                    choices = rev(unique(dataResults$lv)),
                    selected = TRUE,
                    inline = TRUE
                ),
                checkboxGroupInput("K",
                    "Discrete levels",
                    inline = TRUE,
                    choices = levels(dataResults$K),
                    selected = levels(dataResults$K)[c(1, 3, 5)]
                ),
                checkboxGroupInput("pj",
                    "Proportion of noise variables",
                    inline = TRUE,
                    choices = unique(dataResults$pj),
                    selected = unique(dataResults$pj)[c(1, 4)]
                ),
            ),
            column(
                4,
                hr(),
                h4("Outcome measures"),
                selectInput("par",
                    "Parameter",
                    choices = levels(dataResults$par),
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
                    choices = levels(dataResults$method),
                    selected = levels(dataResults$method)[c(1, 3:5)],
                    inline = TRUE
                ),
                shinyWidgets::sliderTextInput(
                    inputId = "npc",
                    label = "Number of principal components (NPC)",
                    hide_min_max = TRUE,
                    choices = sort(unique(dataResults$npc)),
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
                    selected = levels(dataResults$method)[4]
                )
            }
        })

        # Plot
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
    }

    # Run app ------------------------------------------------------------------

    shinyApp(ui, server)

}

#' plotMids
#'
#' Starts a Shiny app to check trace plots for multiple imputation convergence for the \href{https://github.com/EdoardoCostantini/mi-spcr}{mi-spcr} project.
#' @param study. A unit character vector taking value "sim" or "fdd" (simulation study and case study, respectively)
#' @details
#' The interface of the Shiny app allows you to change the values of the following simulation study experimental factors:
#'
#' - Missing data treatment used (see names in the interface):.cols
#' 
#'      - for the simulation study:
#'
#'          - MIMI: mice with minimal missing data models (only variables under imputation used as predictors in the imputation models)
#'          - MIOP: mice using the quickpred function from the R package mice (Van Buuren and Groothuis-Oudshoorn, 2011) to select the predictors for the univariate imputation models via the correlation-based threshold
#'          - MIOR: mice with oracle knowledge on which predictors to use (the univariate imputation models included the other variables under imputation and the predictors that were used to impose missingness)
#'          - aux: mi-pca using pca only on auxiliary variables
#'          - vbv: mi-pca using pca only on a variable-by-variable basis
#'          - all: mi-pca using pca on all data, after a round of single imputation
#' 
#'      - for the case study:
#' 
#'          - expert: mice specified by an expert
#'          - si4auxall: convergence check for single imputation used for pre-processing for mi-pca-all
#'          - pcraux: mi-pca using pca only on auxiliary variables
#'          - vbv: mi-pca using pca only on a variable-by-variable basis
#'          - default: mice specified with default arguments
#'
#' - Imputed variable
#' - Number of iterations used
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
plotMids <- function(study = c("sim", "fdd")[1]) {

    if (study == "sim") {
        mids_interest <- dataMids$sim

        # Method names
        methods <- c("MIMI", "MIOP", "MIOR", "aux", "vbv")

        # Plot tag
        plot_tag <- " for the simulation study"
    }
    if (study == "fdd") {
        mids_interest <- dataMids$fdd

        # Method names
        methods <- c("expert", "si4auxall", "pcraux", "vbv", "default")

        # Plot tag
        plot_tag <- " for the fireworks disaster study"
    }

    # UI -----------------------------------------------------------------------

    ui <- fluidPage(
        h1(paste0(" Trace plots for convergence checks", plot_tag)),
        fluidRow(

            # Missing data treatments ------------------------------------------
            column(
                3,
                hr(),
                selectInput("method",
                    "Imputation method:",
                    choices = methods,
                    selected = methods[1]
                ),
            ),

            # Number of pcs ----------------------------------------------------

            column(
                3,
                hr(),
                selectInput(
                    inputId = "var",
                    label = "Variable",
                    choices = rownames(mids_interest[[1]]$chainMean[,,1]),
                    selected = rownames(mids_interest[[1]]$chainMean[, , 1])[1]
                ),
            ),

            # Number of iterations ---------------------------------------------

            column(
                3,
                hr(),
                shinyWidgets::sliderTextInput(
                    inputId = "iters",
                    label = "Iteration range",
                    hide_min_max = TRUE,
                    choices = 0:100,
                    selected = c(0, 25),
                    grid = FALSE
                ),
            ),
        ),
        hr(),
        plotOutput("plot"),

        # Silent extraction of size
        shinybrowser::detect(),
    )

    # Server -------------------------------------------------------------------

    server <- function(input, output, session) {

        output$plot <- renderPlot(
            res = 96,
            height = 750,
            {
                # Define condition to plot based on inputs
                if (study == "sim") {
                    cnd_id <- grep(input$method, names(mids_interest))
                    # cnd_id <- grep(methods[1], names(mids_interest))
                }
                if (study == "fdd") {
                    cnd_id <- grep(input$method, names(mids_interest))
                }

                # Work with simple object name
                x <- mids_interest[[cnd_id]]

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
                varlist <- input$var
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
                    xlim = c(input$iters[1] - 1, input$iters[2] + 1),
                    xlab = "Iteration",
                    ylab = "",
                    strip = strip.combined,
                    par.strip.text = list(lines = 0.5),
                )
            }
        )
    }

    # Run app ------------------------------------------------------------------

    shinyApp(ui, server)

}