# Project:   plotmipca
# Objective: Function to run app
# Author:    Edoardo Costantini
# Created:   2022-12-02
# Modified:  2023-04-27
# Notes:

#' start_app
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
start_app <- function() {
    shinyApp(
        ui = ui_call(),
        server = server
    )
}
