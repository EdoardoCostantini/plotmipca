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
