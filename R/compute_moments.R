# these functions compute the mean and standard deviation of distribution objects

#' @export
compute_moments.beta_dist <- function(dist) {
    a <- dist[['alpha']]; b <- dist[['beta']]
    return(list(mu = a / (a + b), sigma = sqrt(a * b / (a + b) ^ 2 / (a + b + 1))))
}

#' @export
compute_moments.normal_gamma_dist <- function(dist) {
    mu <- dist[['mu']]; lambda <- dist[['lambda']]
    a <- dist[['alpha']]; b <- dist[['beta']]
    return(list(x = list(mu = mu, sigma = sqrt(b / lambda / (a - 1))), tau = list(mu = a / b, sigma = sqrt(a / b ^ 2))))
}

#' @export
compute_moments.gamma_dist <- function(dist) {
    a <- dist[['alpha']]; b <- dist[['beta']]
    return(list(mu = a / b, sigma = sqrt(a / b ^ 2)))
}

#' @title Simulate Data According to Some Distribution
#' @name simulate_data
#' @description Simulate a vector of data from a given distribution object.
#' @export
#' @param dist An object of class \code{'beta_dist'}, \code{'normal_gamma_dist'},
#'             \code{'gamma_dist'} that specifies the parameters of some distribution
#' @return A list
compute_moments <- function(dist) {
    UseMethod('compute_moments')
}