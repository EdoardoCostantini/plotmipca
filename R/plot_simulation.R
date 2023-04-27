#' Plot simulation results
#'
#' Generate the main plot for the simulation study.
#'
#' @param results object containing results produced by the simulation study
#' @param n_items experimental factor value: number of items
#' @param parameter estimated parameter of interest
#' @param latent_structure experimental factor value: whether the latent structure is imposed or not
#' @param method_vector experimental factor value: imputation methods considered
#' @param npc_range experimental factor value: number of components considered
#' @param categories experimental factor value: number of categories of discretized variables
#' @param outcome performance measure to plot
#' @return Returns the ggplot
#' @author Edoardo Costantini, 2023
#' @examples
#' # Define example inputs
#' results <- dataResults
#' n_items <- unique(dataResults$j)[1]
#' parameter <- levels(dataResults$par)[15]
#' latent_structure <- unique(dataResults$lv)[2]
#' method_vector <- levels(dataResults$method)[c(1, 3:5)]
#' npc_range <- c(0, 10)
#' categories <- levels(dataResults$K)[c(1, 3, 5)]
#' prop_noise <- unique(dataResults$pj)[c(1, 4)]
#' outcome <- c("bias", "CIC", "CIW", "mcsd")[1]
#' 
#' # Use the function
#' plot_simulation(
#'     results = dataResults,
#'     n_items = unique(dataResults$j)[1],
#'     parameter = levels(dataResults$par)[15],
#'     latent_structure = unique(dataResults$lv)[2],
#'     method_vector = levels(dataResults$method)[c(1, 3:5)],
#'     npc_range = c(0, 10),
#'     categories = levels(dataResults$K)[c(1, 3, 5)],
#'     prop_noise = unique(dataResults$pj)[c(1, 4)],
#'     outcome = c("bias", "CIC", "CIW", "mcsd")[1]
#' )
#' 
#' @export
plot_simulation <- function(results, n_items, parameter, latent_structure, method_vector, npc_range, categories, prop_noise, outcome) {

    # Filter the data as requested
    results_filtered <- results %>%
        filter(
            j == n_items,
            par == parameter,
            lv == latent_structure,
            method %in% method_vector,
            npc <= npc_range[2],
            npc >= npc_range[1],
            K %in% categories,
            pj %in% prop_noise
        )

    # Make NPCs a factors
    results_ready <- results_filtered %>% 
        mutate(npc = factor(npc))
        
    # Make plot
    results_ready %>%
        ggplot(aes_string(
            x = "K",
            y = outcome,
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
            y = outcome
        )
}