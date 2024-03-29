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
                "Solving the many variables problem in MICE with PCR",
                align = "center"
            )
        ),
        shiny::column(
            width = 10,
            offset = 1,
            # Create tabs for different plotting aspects
            shiny::tabsetPanel(
                type = "tabs",
                selected = "About this Shiny app",
                shiny::tabPanel(
                    title = "About this Shiny app",
                    shiny::column(
                        width = 8,
                        offset = 2,
                        shiny::HTML(
                            "<br>
                            This Shiny app accompanies the paper:
                            <br>
                            <br>
                            Costantini, E., Lang, K.M., Sijtsma, K., Reeskens, T. (2023). Solving the many-variables problem in MICE with principal component regression. <i>Behav Res</i>. <a href='https://doi.org/10.3758/s13428-023-02117-1'>https://doi.org/10.3758/s13428-023-02117-1</a>
                            <br>
                            <br>
                            With this app, the user can:
                            <ul>
                                <li>Interact with the simulation study results presented in the paper by using <b>Module 1</b>.</li>
                                <li>Interact with the case study results presented in the paper by using <b>Module 2</b>.</li>
                                <li>Interact with trace plots showing the convergence of the methods compared in the simulation study by using <b>Module 3</b>.</li>
                                <li>Interact with trace plots showing the convergence of the methods compared in the case study by using <b>Module 4</b>.</li>
                            </ul>
                            For questions and feedback, feel free to <a href = 'mailto:e.costantini@tilburguniversity.edu'>send me an email</a>.
                            "
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 1: Simulation study",
                    shiny::fluidRow(
                        shiny::column(
                            width = 3,
                            # Simulation study: Description --------------------
                            shiny::HTML(
                                "<br>
                                    This tab allows you to plot the results of the simulation study reported in the article.
                                    You change the values of the experimental factors to plot the results you are most interested in.
                                    <br>
                                    <br>"
                            ),
                            shiny::navlistPanel(
                                widths = c(11, 12),
                                shiny::tabPanel(
                                    title = "1. Data generation",
                                    shiny::radioButtons("j",
                                        "Number of observed items",
                                        choices = unique(plotmipca::dataResults$j),
                                        selected = unique(plotmipca::dataResults$j)[1],
                                        inline = TRUE
                                    ),
                                    shiny::radioButtons("lv",
                                        "Latent structure",
                                        choices = rev(unique(plotmipca::dataResults$lv)),
                                        selected = TRUE,
                                        inline = TRUE
                                    ),
                                    shiny::checkboxGroupInput("K",
                                        "Number of categories",
                                        inline = TRUE,
                                        choices = levels(plotmipca::dataResults$K),
                                        selected = levels(plotmipca::dataResults$K)[c(1, 3, 5)]
                                    ),
                                    shiny::checkboxGroupInput("pn",
                                        "Proportion of noise variables (pn)",
                                        inline = TRUE,
                                        choices = unique(plotmipca::dataResults$pn),
                                        selected = unique(plotmipca::dataResults$pn)
                                    )
                                ),
                                shiny::tabPanel(
                                    title = "2. Missing data treatments",
                                    shiny::checkboxGroupInput("method",
                                        "Methods",
                                        choices = levels(plotmipca::dataResults$method),
                                        selected = levels(plotmipca::dataResults$method)[c(1, 3:5, 8)],
                                        inline = FALSE
                                    ),
                                    shinyWidgets::sliderTextInput(
                                        inputId = "npc",
                                        label = "Number of principal components (NPC)",
                                        hide_min_max = TRUE,
                                        choices = sort(unique(plotmipca::dataResults$npc)),
                                        selected = c(0, 10),
                                        grid = TRUE
                                    )
                                ),
                                shiny::tabPanel(
                                    title = "3. Simulation outcomes",
                                    shiny::selectInput("par",
                                        "Parameter",
                                        choices = levels(plotmipca::dataResults$par),
                                        selected = "x1 correlation x2"
                                    ),
                                    shiny::radioButtons("plot_y_axis",
                                        "Outcome measure",
                                        choices = c("PRB", "CIC", "CIW"),
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
                    title = "Module 2: Case study",
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
                            shiny::selectInput("res_case_dv",
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
                    title = "Module 3: Simulation study convergence",
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
                            shiny::selectInput("conv_sim_method",
                                "Imputation method:",
                                choices = c("MI-PCR-AUX", "MI-PCR-VBV", "MI-MI", "MI-OP", "MI-OR"),
                                selected = "MI-PCR-AUX"
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
                    title = "Module 4: Case study convergence",
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
                            shiny::selectInput("conv_case_method",
                                "Imputation method:",
                                choices = c("MI-PCR-AUX", "MI-PCR-VBV", "SI-PCR-ALL", "Default", "Expert"),
                                selected = "MI-PCR-AUX"
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