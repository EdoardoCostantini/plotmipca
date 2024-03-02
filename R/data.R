# Project:   plotmipca
# Objective: container for data documentation
# Author:    Edoardo Costantini
# Created:   2022-12-17
# Modified:  2023-04-30

#' dataResults
#'
#' The data.frame containing the results of the simulation study. It is automatically called by the plotting function.
#'
#' The columns of the data.frame are
#'
#' \itemize{
#'   \item j. Total number of items involved
#'   \item K. Discretization level (number of categories)
#'   \item D. Proportion of discretized variables
#'   \item interval. Whether data was discretized to interval or ordinal scale
#'   \item pn. Proportion of noise variables
#'   \item lv. Presence of latent structure
#'   \item par. Parameter of interest
#'   \item npc. Number of pcs requested
#'   \item method. Missing data treatment used
#'   \item Mean. Average estimate of the statistic of interest
#'   \item mcsd. Standard deviation of the statistic estimate over the Monte Carlo repetitions
#'   \item PC_exp. Explained variance by the extracted PCs
#'   \item ref. True value of the statistic
#'   \item bias. Raw bias
#'   \item CIC. Confidence interval coverage
#'   \item CIW. Average confidence interval width
#'   \item CIW_sd. Standard deviation of the confidence interval width
#'   \item lwr_avg. Average value of the confidence interval lower bound
#'   \item upr_avg. Average value of the confidence interval upper bound
#'   \item NPC. Number of PCs used by method
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dataResults
#' @usage data(dataResults)
#' @format A data frame with 12108 rows and 21 columns
NULL

#' mids_sim
#'
#' A list of (simplified) mids objects and condition descriptions used to obtain the trace plots for desired conditions.
#'
#' @docType data
#' @keywords datasets
#' @name mids_sim
#' @usage data(mids_sim)
#' @format A list containing simplified mids for the simulation study
NULL

#' mids_case
#'
#' A list of (simplified) mids objects and condition descriptions used to obtain the trace plots for desired conditions.
#'
#' @docType data
#' @keywords datasets
#' @name mids_case
#' @usage data(mids_case)
#' @format A list containing simplified mids for the case study
NULL