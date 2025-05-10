#' Multilevel Omega Estimation
#'
#' Estimates McDonald's omega coefficients for multilevel data using a multilevel structural equation model (SEM).
#'
#' @param data A data frame containing item responses.
#' @param items A character vector of column names representing the items for a single scale.
#' @param id A column name (as string) indicating the clustering/grouping variable (e.g., participant ID).
#' @param savemodel Logical. If TRUE, the SEM model will be saved in the results. Default is TRUE.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{within}{Estimate of omega within-group consistency.}
#'   \item{between}{Estimate of omega between-group consistency.}
#'   \item{omega_fit}{Model fit statistics for the SEM.}
#' }
#'
#' @examples
#' # Simulate item responses for one scale
#' set.seed(17081945)
#' sim_data <- data.frame(participant_id = rep(1:100, each = 5))
#' for (i in 1:4) {
#'   sim_data[[paste0("A_item", i)]] <- rnorm(500, 5, 1)
#' }
#'
#' # Run omega estimation
#' get_omega(sim_data, items = paste0("A_item", 1:4), id = "participant_id")
#'
#' @import multilevelTools
#' @export
get_omega <- function(data, items, id, savemodel = TRUE) {
  omega_estimates <- multilevelTools::omegaSEM(
    items = items,
    id = id,
    data = data,
    savemodel = savemodel
  )

  omega_results <- omega_estimates$Results
  omega_fit     <- omega_estimates$Fit

  return(
    list(
      within = omega_results[omega_results$label == "omega_within", ]$est,
      between = omega_results[omega_results$label == "omega_between", ]$est,
      omega_fit = omega_fit
    )
  )
}


#' Multilevel Omega for Multiple Scales
#'
#' Applies multilevel omega estimation to multiple sets of items defined in a named list, returning both within and between omega coefficients for each scale.
#'
#' @param data A data frame containing item responses.
#' @param item_vectors A named list where each element is a character vector of item names corresponding to a scale.
#' @param id A column name (as string) indicating the clustering/grouping variable (e.g., participant ID).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{scales}{The name of each scale.}
#'   \item{omega_within}{Within-group omega for that scale.}
#'   \item{omega_between}{Between-group omega for that scale.}
#' }
#'
#' @examples
#' # Simulate some multilevel item data
#' set.seed(17081945)
#' sim_data <- data.frame(participant_id = rep(1:100, each = 5))
#'
#' for (i in 1:4) {
#'   sim_data[[paste0("A_item", i)]] <- rnorm(500, 5, 1)
#'   sim_data[[paste0("B_item", i)]] <- rnorm(500, 5, 1)
#' }
#'
#' # Define item sets
#' item_sets <- list(
#'    scale_A = c("A_item1", "A_item2", "A_item3", "A_item4"),
#'    scale_B = c("B_item1", "B_item2", "B_item3", "B_item4")
#' )
#'
#' # Compute multilevel omega
#' get_omega_all_sets(sim_data, item_sets, id = "participant_id")
#'
#' @import purrr
#' @import multilevelTools
#' @export
get_omega_all_sets <- function(data, item_vectors, id) {
  result <- purrr::map_dfr(names(item_vectors), function(label) {
    items <- item_vectors[[label]]
    omega_estimates <- get_omega(data, items, id)
    data.frame(scales = label,
               omega_within = omega_estimates$within,
               omega_between = omega_estimates$between)
  })

  return(result)
}
