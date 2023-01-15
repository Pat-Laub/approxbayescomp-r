general_sample_data <- function(params, n) {
  #' Generates a vector of i.i.d. random sums
  #'
  #' @param params List of distributional arguments for the sum and summand distributions
  #' @param n Number of samples to generate
  #' @return Vector of i.i.d. random sums
  
  # Extract sum and summand distribution parameters
  sum_distribution <- params$sum_distribution
  summand_distribution <- params$summand_distribution
  
  # Create sampling functions
  sum_sampling_fun <- match.fun(paste0("r", sum_distribution$name))
  summand_sampling_fun <- match.fun(paste0("r", summand_distribution$name))
  
  # Initialize empty vector to store samples
  samples <- numeric(n)
  
  # Iterate over number of samples
  for(i in 1:n) {
    # Sample summand
    summand <- summand_sampling_fun(summand_distribution$args)
    # Sample sum
    sum <- summand + sum_sampling_fun(sum_distribution$args)
    # Append sum to samples
    samples[i] <- sum
  }
  
  return(samples)
}

