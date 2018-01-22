#' @export
#' @importFrom stats rbeta
get_data_dists.beta_dist <- function(dist, n, num_variants, ...) {
    rates <- stats::rbeta(n = n * num_variants, dist[['alpha']], dist[['beta']])
    return(purrr::map2(rates[1:n], rates[(n + 1):length(rates)]
                       , function(x, y) list(a = bernoulli_dist(rate = x)
                                             , b = bernoulli_dist(rate = y))))
}

#' @export
#' @importFrom stats rgamma rnorm
get_data_dists.normal_gamma_dist <- function(dist, n, num_variants, ...) {
    tau <- stats::rgamma(n = n * num_variants, shape = dist[['alpha']], rate = dist[['beta']])
    mu <- stats::rnorm(n = n * num_variants, mean = dist[['mu']], sd = sqrt(1 / (dist[['lambda']] * tau)))
    joint <- purrr::map2(mu, tau, c)
    return(purrr::map2(joint[1:n], joint[(n + 1):length(joint)]
                       , function(x, y) list(a = normal_dist(mu = x[1], sigma = sqrt(1 / x[2]))
                                             , b = normal_dist(mu = y[1], sigma = sqrt(1 / y[2])))))
}

#' @export
#' @importFrom stats rgamma
get_data_dists.gamma_dist <- function(dist, n, num_variants, ...) {
    rates <- stats::rgamma(n = n * num_variants, dist[['alpha']], dist[['beta']])
    return(purrr::map2(rates[1:n], rates[(n + 1):length(rates)]
                       , function(x, y) list(a = poisson_dist(rate = x)
                                             , b = poisson_dist(rate = y))))
}

#' @title Sample Data Distributions
#' @name get_data_dists
#' @description Given a prior distribution, sample many data generating distributions
#' @param dist A prior distribution
#' @param n The number of distributions to sample
#' @param num_variants The number of variants to use
#' @param ... Arguments to be used by other methods
#' @export
#' @return A list of data generating distributions for multiple variants
get_data_dists <- function(dist, n, num_variants, ...) {
    UseMethod('get_data_dists')
}
