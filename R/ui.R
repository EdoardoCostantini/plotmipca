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
                            shiny::HTML(
                                "<br>
                                    This tab allows you to plot the results of the simulation study.
                                    You change the values of the experimental factors to plot the results you are most interested in.
                                    <br>
                                    <br>"
                            ),
                            shiny::navlistPanel(
                                widths = c(11, 12),
                                shiny::tabPanel(
                                    title = "1. Data generation",
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
                                        selected = unique(dataResults$pj)
                                    )
                                ),
                                shiny::tabPanel(
                                    title = "2. Missing data treatments",
                                    checkboxGroupInput("method",
                                        "Methods",
                                        choices = levels(dataResults$method),
                                        selected = levels(dataResults$method)[c(1, 3:5, 8)],
                                        inline = TRUE
                                    ),
                                    shinyWidgets::sliderTextInput(
                                        inputId = "npc",
                                        label = "Number of principal components (NPC)",
                                        hide_min_max = TRUE,
                                        choices = sort(unique(dataResults$npc)),
                                        selected = c(0, 10),
                                        grid = TRUE
                                    )
                                ),
                                shiny::tabPanel(
                                    title = "3. Simulation outcomes",
                                    selectInput("par",
                                        "Parameter",
                                        choices = levels(dataResults$par),
                                        selected = "z1 correlation z2"
                                    ),
                                    radioButtons("plot_y_axis",
                                        "Outcome measure",
                                        choices = c("bias", "CIC", "CIW", "mcsd"),
                                        inline = TRUE
                                    )
                                )
                            )
                        ),
                        shiny::column(
                            width = 9,
                            shiny::plotOutput("plot"),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Simulation study: convergence checks",
                    shiny::fluidRow(
                        shiny::column(
                            width = 3,
                            shiny::HTML(
                                "<br>
                                This tab allows you to interact with the trace plots for the imputation methods used in the simulation study.
                                <br>
                                <br>
                                "
                            ),
                            selectInput("conv_sim_method",
                                "Imputation method:",
                                choices = c("MI-PCA-AUX", "MI-PCA-VBV", "MI-MI", "MI-OP", "MI-OR"),
                                selected = "MI-PCA-AUX"
                            ),
                            shinyWidgets::sliderTextInput(
                                inputId = "conv_sim_iters",
                                label = "Iteration range",
                                hide_min_max = TRUE,
                                choices = 0:100,
                                selected = c(0, 25),
                                grid = FALSE
                            )
                        ),
                        shiny::column(
                            width = 9,
                            shiny::plotOutput("mids_sim_plot"),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Case study",
                    shiny::fluidRow(
                        shiny::column(
                            width = 3,
                            shiny::HTML(
                                "<br>
                                This tab allows you to plot the results of the case study reported in the article.
                                <br>
                                <br>
                                "
                            ),
                            selectInput("res_case_dv",
                                "Dependent variable",
                                choices = c("PTSD-RI parent score", "PTSD-RI children score"),
                                selected = "PTSD-RI parent score"
                            )
                        ),
                        shiny::column(
                            width = 9,
                            shiny::plotOutput("case_plot_res"),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Case study: convergence checks",
                    shiny::fluidRow(
                        shiny::column(
                            width = 3,
                            # Case study convergence: description --------------
                            shiny::HTML(
                                "<br>
                                This tab allows you to interact with the trace plots for the imputation methods used in the case study.
                                <br>
                                <br>
                                "
                            ),
                            selectInput("conv_case_method",
                                "Imputation method:",
                                choices = c("MI-PCA-AUX", "MI-PCA-VBV", "SI-PCA-ALL", "Default", "Expert"),
                                selected = "MI-PCA-AUX"
                            ),
                            shinyWidgets::sliderTextInput(
                                inputId = "conv_case_iters",
                                label = "Iteration range",
                                hide_min_max = TRUE,
                                choices = 0:100,
                                selected = c(0, 25),
                                grid = FALSE
                            ),
                        ),
                        shiny::column(
                            width = 9,
                            shiny::plotOutput("mids_case_plot"),
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