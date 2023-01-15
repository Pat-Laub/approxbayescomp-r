# library(statistics)

euclidean_distance <- function(sim_data, obs_data) {
  #' Calculates the Euclidean distance between simulated data and observed data
  #'
  #' @param sim_data Simulated data
  #' @param obs_data Observed data
  #' @return Euclidean distance
  #' @export
  if (length(sim_data) != length(obs_data)) {
    stop("sim_data and obs_data must have the same length")
  }
  return(sqrt(sum((sim_data - obs_data)^2)))
}


wasserstein_distance <- function(sim_data, obs_data) {
  #' Calculates the empirical Wasserstein distance between simulated data and observed data
  #'
  #' @param sim_data Simulated data
  #' @param obs_data Observed data
  #' @return Empirical Wasserstein distance
  #' @export
  if (length(sim_data) != length(obs_data)) {
    stop("sim_data and obs_data must have the same length")
  }
  return(wasserman.test(sim_data, obs_data)$statistic)
}
