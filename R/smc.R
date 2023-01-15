
sample_population_from_prior <- function(n_particles, sampling_functions,
                                         distance_function, obs_data, simulate_data) {
  #' Samples particles from the prior distribution, simulates fake data from the parameters, 
  #' calculates the distance between the fake data and the observed data, and returns a list of particles.
  #' 
  #' @param n_particles Number of particles to sample
  #' @param sampling_functions List of sampling functions for each parameter
  #' @param distance_function Function that calculates the distance between the simulated data and the observed data
  #' @param obs_data Observed data
  #' @param simulate_data Function that simulates data from a set of parameters
  #' @return A list of particles, each containing the parameters, weight, and distance
  
  # Initialize empty list to store particles
  population <- list()
  
  d <- length(obs_data)
  
  # Iterate over number of particles
  for(i in 1:n_particles) {
    # Sample parameter values from prior distributions
    params <- as.numeric(lapply(sampling_functions, function(sampling_fun) sampling_fun()))
    # Simulate fake data from the sampled parameters
    fake_data <- simulate_data(params, 1, d)
    # Calculate distance between fake data and observed data
    distance <- distance_function(fake_data, obs_data)
    # Initialize weight of particle to 1
    weight <- 1
    # Append particle (parameters, weight and distance) to population
    population[[i]] <- list(params=params, weight=weight, distance=distance)
  }
  
  return(population)
}

subset_population <- function(population, quantile = 0.9) {
  #' Subset of the population where the distances are less than the quantile of all distances
  #'
  #' @param population List of particles
  #' @param quantile Quantile threshold for distance
  #' @return Subset of the population and the quantile threshold
  
  distances <- sapply(population, function(p) p$distance)
  threshold <- quantile(distances, quantile)
  subset <- population[sapply(population, function(p) p$distance < threshold)]
  
  return(list(subset = subset, quantile = threshold))
}

library(ks)

fit_kde <- function(population) {
  #' Fits a kernel density estimate to the params part of each particle in the subset population
  #'
  #' @param population A population of theta particles.
  #' @return Fitted kernel density estimate
  
  params_matrix <- do.call(rbind, lapply(population, function(particle) as.numeric(particle$params)))
  
  dim <- length(params_matrix[1,])
  
  k <- kde(params_matrix) #, n = 50, d = dim)
  
  return(k)
}

sample_population_from_kde <- function(n_particles, kde, density_functions, distance_function, obs_data, simulate_data, epsilon = Inf) {
  #' Samples a population of particles from the kde
  #'
  #' @param kde Kernel density estimate
  #' @param density_functions List of density functions for each parameter
  #' @param n_particles Number of particles to sample
  #' @param obs_data Observed data
  #' @param distance_function Function to calculate the distance between the fake data and the observed data
  #' @param epsilon Maximum distance for a particle to be accepted
  #' @return List of particles with params, weight, and distance
  
  particles <- list()
  n_accepted <- 0
  d <- length(obs_data)
  
  while(n_accepted < n_particles){
    params <- rkde(1, kde)
    
    weight <- 1
    for (j in 1:length(density_functions)) {
      weight <- weight * density_functions[[j]](params[[j]])
    }
    if (weight == 0) {
      next
    }
    
    fake_data <- simulate_data(params, 1, d)
    distance <- distance_function(fake_data, obs_data)
    
    if(distance < epsilon){
      weight <- weight / dkde(as.numeric(params), kde)
      particles[[n_accepted+1]] <- list(params=params, weight=weight, distance=distance)
      n_accepted <- n_accepted+1
    }
  }
  
  return(particles)
}

smc <- function(n_particles, priors, simulate_data, obs_data, distance_function, epsilon = Inf, n_iter = 1) {
  #' Sequential Monte Carlo algorithm
  #'
  #' @param n_particles Number of particles to sample
  #' @param priors List of prior distributions
  #' @param simulate_data Function to simulate data from parameters
  #' @param obs_data Observed data
  #' @param distance_function Function to calculate the distance between the fake data and the observed data
  #' @param epsilon Maximum distance for a particle to be accepted
  #' @param n_iter Number of iterations
  #' @return List of populations
  
  sampling_functions <- create_sampling_functions(priors)
  density_functions <- create_density_functions(priors)
  
  population <- sample_population_from_prior(n_particles, sampling_functions,
                                             distance_function, obs_data, simulate_data)
  
  populations <- list(population)
  
  for (i in 1:n_iter) {
    subset_pop <- subset_population(population)
    new_population <- subset_pop$subset
    epsilon <- subset_pop$quantile
    kde <- fit_kde(new_population)
    population <- sample_population_from_kde(n_particles, kde, density_functions,
                                             distance_function, obs_data, simulate_data, epsilon)
    populations[[i+1]] <- population
  }
  
  return(populations)
}


