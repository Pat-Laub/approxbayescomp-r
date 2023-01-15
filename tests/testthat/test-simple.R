test_that("Simple simulator", {
  testthat::expect_true(TRUE)
})

# Test distance (TODO)

# Test Prior

priors <- list(list(name = "norm", args = list(mean = 5, sd = 2)),
               list(name = "unif", args = list(min = 0, max = 1)))

sampling_functions <- create_sampling_functions(priors)
density_functions <- create_density_functions(priors)
sampling_functions[[1]]()
density_functions[[1]](2)


# Test simulator

poiss_exp_sums <- function(params, n, d) {
  #' Generates a matrix of i.i.d. random sums
  #'
  #' @param params List of distributional arguments for the sum and summand distributions
  #' @param n Number of samples to generate
  #' @param d Number of random sums per sample
  #' @return Matrix of shape n by d of i.i.d. random sums
  
  # Extract sum and summand distribution parameters
  sum_distribution_name <- "pois" # params$sum_distribution
  summand_distribution_name <- "exp" # params$summand_distribution
  
  sum_distribution_arg <- params[[1]]
  summand_distribution_arg <- params[[2]]
  
  # Create sampling functions
  sum_sampling_fun <- match.fun(paste0("r", sum_distribution_name))
  summand_sampling_fun <- match.fun(paste0("r", summand_distribution_name))
  
  # Initialize empty matrix to store samples
  samples <- matrix(numeric(n*d), n, d)
  
  # Iterate over number of samples
  for(i in 1:n) {
    for(j in 1:d){
      num_summands <- sum_sampling_fun(1, sum_distribution_arg)
      samples[i,j] <- sum(summand_sampling_fun(num_summands, summand_distribution_arg))
    }
  }
  
  return(samples)
}

poiss_exp_sums(as.numeric(c(10, 20)), 3, 2)


# Test SMC

set.seed(1234)

n_particles <- 1000
dist <- euclidean_distance

simulate_data <- poiss_exp_sums

true_theta <- c(2, 8)
d <- 100
obs_data <- simulate_data(true_theta, 1, d)

priors <- list(list(name = "exp", args = list(rate = 0.1)),
               list(name = "unif", args = list(min = 0, max = 10)))


sampling_functions <- create_sampling_functions(priors)
density_functions <- create_density_functions(priors)

result <- smc(n_particles, priors, simulate_data, obs_data, dist, epsilon = epsilon, n_iter = 10)


# 
# 
# population <- sample_population_from_prior(n_particles, sampling_functions,
#                                 dist, obs_data, simulate_data)
# 
# ss <- subset_population(population)
# new_population <- ss$subset
# epsilon <- ss$quantile
# 
# k <- fit_kde(new_population)
# 
# next_population <- sample_population_from_kde(n_particles, k,
#                         density_functions, dist, obs_data, simulate_data, epsilon = epsilon)
# 




library(ggplot2)

result_matrices <- lapply(result, function(pop) {
  do.call(rbind, lapply(pop, function(p) p$params))
})

for (col in 1:2) {
  
  print(col)
  result_matrices_column <- lapply(result_matrices, function(matrix) matrix[,col])
  
  
  # create a data frame with one column for the iteration number and one column for the first element of the params
  df <- data.frame(iteration = rep(1:length(result_matrices_column), sapply(result_matrices_column, length)),
                   params = unlist(result_matrices_column))
  
  # plot the KDEs
  print(ggplot(df, aes(params, color = factor(iteration))) + 
          geom_density(alpha = 0.5) + 
          scale_fill_gradient(low = "blue", high = "red") +
          xlab("First element of params") + 
          ylab("Density") + 
          ggtitle("Sequence of KDEs for the n-th element of the params") + 
          theme_classic())
}
