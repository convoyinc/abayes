#' @export
#' @importFrom stats rbeta
simulate_data.beta_dist <- function(dist, n, ...) {
    return(stats::rbeta(n = n, dist[['alpha']], dist[['beta']]))
}

#' @export
#' @importFrom stats rgamma rnorm
simulate_data.normal_gamma_dist <- function(dist, n, ...) {
    tau <- stats::rgamma(n = n, shape = dist[['alpha']], rate = dist[['beta']])
    return(stats::rnorm(n = n, mean = dist[['mu']], sd = sqrt(1 / (dist[['lambda']] * tau))))
}

#' @export
#' @importFrom stats rgamma
simulate_data.gamma_dist <- function(dist, n, ...) {
    return(stats::rgamma(n = n, shape = dist[['alpha']], rate = dist[['beta']]))
}

#' @export
#' @importFrom stats rbinom
simulate_data.bernoulli_dist <- function(dist, n, ...) {
    return(as.numeric(stats::rbinom(n, size = 1, prob = dist[['rate']])))
}

#' @export
#' @importFrom stats rnorm
simulate_data.normal_dist <- function(dist, n, ...) {
    return(stats::rnorm(n, mean = dist[['mu']], sd = dist[['sigma']]))
}

#' @export
#' @importFrom stats rpois
simulate_data.poisson_dist <- function(dist, n, ...) {
    return(as.numeric(stats::rpois(n, lambda = dist[['rate']])))
}

#' @title Simulate Data According to Some Distribution
#' @name simulate_data
#' @description Simulate a vector of data from a given distribution object.
#' @export
#' @param dist An object of class \code{'beta_dist'}, \code{'normal_gamma_dist'},
#'             \code{'gamma_dist'}, \code{'bernouilli_dist'}, \code{'normal_dist'},
#'             or \code{'poisson_dist'} that specifies the parameters of some distribution
#' @param n How many data points to simulate
#' @param ... Arguments to be passed onto other methods
#' @return A vector of simulated data.
simulate_data <- function(dist, n, ...) {
    UseMethod('simulate_data')
}

