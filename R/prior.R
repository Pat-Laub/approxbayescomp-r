create_sampling_functions <- function(dists) {
  lapply(dists, function(dist) {
    sample_fun <- match.fun(paste0("r", dist$name))
    return(function() {
      do.call(sample_fun, c(n = 1, dist$args))
    })
  })
}

create_density_functions <- function(dists) {
  lapply(dists, function(dist) {
    density_fun <- match.fun(paste0("d", dist$name))
    return(function(x) {
      do.call(density_fun, c(x, dist$args))
    })
  })
}
