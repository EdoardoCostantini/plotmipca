#' User interface call
#'
#' Calls the definition of the user interface and returns it as an output
#'
#' @return UI object that can be passed directly to shiny::shinyApp()
#' @author Edoardo Costantini, 2023
#' @export
ui_call <- function() {
    # Define ui object
    ui <- shiny::fluidPage(

        # App title
        shiny::titlePanel(
            shiny::h1(
                'Solving the "many variables" problem in MICE with principal component regression',
                align = "center"
            )
        ),
        shiny::column(
            width = 10,
            offset = 1,
            # Create tabs for different plotting aspects
            shiny::tabsetPanel(
                type = "tabs",
                selected = "Simulation study",
                shiny::tabPanel(
                    title = "Simulation study",
                    shiny::fluidRow(
                        shiny::column(
                            width = 3,
                            # Simulation study: Description --------------------
                            shiny::titlePanel(
                                shiny::h3("Simulation study", align = "center")
                            ),
                            shiny::tabsetPanel(
                                type = "tabs",
                                shiny::tabPanel(
                                    title = "Introduction",
                                    "Coming Soon"
                                ),
                                shiny::tabPanel(
                                    title = "1. Setup",
                                    "Coming Soon"
                                )
                            )
                        ),
                        shiny::column(
                            width = 9,
                            # Simulation study: inputs -------------------------
                            shiny::fluidRow(
                                shiny::titlePanel(
                                    shiny::h3("Inputs", align = "center")
                                ),
                                column(
                                    4,
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
                            # Simulation study: Output -------------------------
                            shiny::fluidRow(
                                shiny::titlePanel(
                                    shiny::h3("Plot", align = "center")
                                ),
                                shiny::plotOutput("plot")
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Simulation study: convergence checks",
                    shiny::fluidRow(
                        shiny::column(
                            width = 3,
                            # Simulation study Convergence: Description --------
                            shiny::titlePanel(
                                shiny::h3("Trace plots fo convergence", align = "center")
                            ),
                            shiny::tabsetPanel(
                                type = "tabs",
                                shiny::tabPanel(
                                    title = "Introduction",
                                    "Coming Soon"
                                ),
                                shiny::tabPanel(
                                    title = "1. Setup",
                                    "Coming Soon"
                                )
                            )
                        ),
                        shiny::column(
                            width = 9,
                            shiny::fluidRow(
                                # Simulation study Convergence: Input ----------
                                shiny::titlePanel(
                                    shiny::h3("Inputs", align = "center")
                                ),
                                column(
                                    4,
                                    selectInput("conv_sim_method",
                                        "Imputation method:",
                                        choices = c("MIMI", "MIOP", "MIOR", "aux", "vbv"),
                                        selected = "MIMI"
                                    ),
                                ),
                                column(
                                    4,
                                    selectInput(
                                        inputId = "conv_sim_var",
                                        label = "Variable",
                                        choices = rownames(dataMids$sim[[1]]$chainMean[, , 1]),
                                        selected = rownames(dataMids$sim[[1]]$chainMean[, , 1])[1]
                                    ),
                                ),
                                column(
                                    4,
                                    shinyWidgets::sliderTextInput(
                                        inputId = "conv_sim_iters",
                                        label = "Iteration range",
                                        hide_min_max = TRUE,
                                        choices = 0:100,
                                        selected = c(0, 25),
                                        grid = FALSE
                                    ),
                                )
                            ),
                            shiny::fluidRow(
                                # Simulation study Convergence: Output ---------
                                shiny::titlePanel(
                                    shiny::h3("Plot", align = "center")
                                ),
                                shiny::plotOutput("mids_sim_plot")
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Case study",
                    "Coming soon"
                ),
                shiny::tabPanel(
                    title = "Case study: convergence checks",
                    shiny::fluidRow(
                        shiny::column(
                            width = 3,
                            # Case study convergence: description --------------
                            shiny::titlePanel(
                                shiny::h3("Trace plots fo convergence", align = "center")
                            ),
                            shiny::tabsetPanel(
                                type = "tabs",
                                shiny::tabPanel(
                                    title = "Introduction",
                                    "Coming Soon"
                                ),
                                shiny::tabPanel(
                                    title = "1. Setup",
                                    "Coming Soon"
                                )
                            )
                        ),
                        shiny::column(
                            width = 9,
                            shiny::fluidRow(
                                # Case study convergence: inputs ----------
                                shiny::titlePanel(
                                    shiny::h3("Inputs", align = "center")
                                ),
                                column(
                                    4,
                                    selectInput("conv_case_method",
                                        "Imputation method:",
                                        choices = c("expert", "si4auxall", "pcraux", "vbv", "default"),
                                        selected = "expert"
                                    ),
                                ),
                                column(
                                    4,
                                    selectInput(
                                        inputId = "conv_case_var",
                                        label = "Variable",
                                        choices = rownames(dataMids$fdd[[1]]$chainMean[, , 1]),
                                        selected = rownames(dataMids$fdd[[1]]$chainMean[, , 1])[1]
                                    ),
                                ),
                                column(
                                    4,
                                    shinyWidgets::sliderTextInput(
                                        inputId = "conv_case_iters",
                                        label = "Iteration range",
                                        hide_min_max = TRUE,
                                        choices = 0:100,
                                        selected = c(0, 25),
                                        grid = FALSE
                                    ),
                                )
                            ),
                            shiny::fluidRow(
                                # Case study convergence: output ---------------
                                shiny::titlePanel(
                                    shiny::h3("Plot", align = "center")
                                ),
                                shiny::plotOutput("mids_case_plot")
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                )
            )
        )
    )

    # Return ui object
    return(ui)
}