#' @title Beta Distribution
#' @name beta_dist
#' @description Create a beta distribution object, used as the prior for metrics that
#'              have a bernoulli distribution.
#' @export
#' @param alpha The non-negative alpha parameter of the beta distribution
#' @param beta The non-negative beta parameter of the beta distribution
#' @return An object of class \code{'beta'}.
beta_dist <- function(alpha, beta) {
    if (!is.numeric(c(alpha, beta))) {
        stop('alpha and beta must be numeric')
    }
    if (alpha < 0 || beta < 0) {
        stop('alpha and beta must be non-negative')
    }
    x <- list('alpha' = alpha, 'beta' = beta)
    class(x) <- 'beta_dist'
    return(x)
}

#' @title Normal-Gamma Distribution
#' @name normal_gamma_dist
#' @description Create a normal_gamma distribution object, used as the prior for
#'              metrics that have a normal distribution.
#' @export
#' @param mu The mean of the normal-gamma distribution
#' @param lambda The non-negative lambda parameter of the normal-gamma distribution
#' @param alpha The non-negative alpha parameter of the normal-gamma distribution
#' @param beta The non-negative beta parameter of the normal-gamma distribution
#' @return A normal_unknown dist object
normal_gamma_dist <- function(mu, lambda, alpha, beta) {
    if (!is.numeric(c(mu, lambda, alpha, beta))) {
        stop('mu, lambda, alpha, and beta must be numeric')
    }
    if (lambda < 0 || alpha < 0 || beta < 0) {
        stop('lambda, alpha, and beta must be non-negative')
    }
    x <- list('mu' = mu, 'lambda' = lambda, 'alpha' = alpha, 'beta' = beta)
    class(x) <- 'normal_gamma_dist'
    return(x)
}

#' @title Gamma Distribution
#' @name gamma_dist
#' @description Create a gamma distribution object, used as the prior for
#'              metrics that have a poisson distribution.
#' @export
#' @param alpha The non-negative alpha (shape) parameter of the gamma distribution
#' @param beta The non-negative beta (rate) parameter of the gamma distribution
#' @return A normal_unknown dist object
gamma_dist <- function(alpha, beta) {
    if (!is.numeric(c(alpha, beta))) {
        stop('alpha and beta must be numeric')
    }
    if (alpha < 0 || beta < 0) {
        stop('alpha and beta must be non-negative')
    }
    x <- list('alpha' = alpha, 'beta' = beta)
    class(x) <- 'gamma_dist'
    return(x)
}

#' @title Bernoulli Distribution
#' @name bernoulli_dist
#' @description Create a bernoulli distribution object, used as the data generating
#'              distribution in tests where the metric is a probability of an
#'              event happening.
#' @export
#' @param rate The rate of the bernoulli distribution (between 0 and 1)
#' @return A \code{bernoulli_dist} object
bernoulli_dist <- function(rate) {
    if (!is.numeric(rate)) {
        stop('rate must be numeric')
    }
    if (rate < 0 || rate > 1) {
        stop('rate must be between 0 and 1')
    }
    x <- list('rate' = rate)
    class(x) <- 'bernoulli_dist'
    return(x)
}

#' @title Normal Distribution
#' @name normal_dist
#' @description Create a normal distribution object, used as the data generating
#'              distribution in tests where the metric is a continuous value.
#' @export
#' @param mu The mean of the normal distribution
#' @param sigma The non-negative standard deviation of the normal distribution
#' @return A \code{normal_dist} object
normal_dist <- function(mu, sigma) {
    if (!is.numeric(c(mu, sigma))) {
        stop('mu and sigma must be numeric')
    }
    if (sigma < 0) {
        stop('sigma must be non-negative')
    }
    x <- list('mu' = mu, 'sigma' = sigma)
    class(x) <- 'normal_dist'
    return(x)
}

#' @title Poisson Distribution
#' @name poisson_dist
#' @description Create a poisson distribution object, used as the data generating
#'              distribution in tests where the metric is a count of an event happening.
#' @export
#' @param rate The non-negative mean of the poisson distribution
#' @return A \code{poisson_dist} object
poisson_dist <- function(rate) {
    if (!is.numeric(rate)) {
        stop('rate must be numeric')
    }
    if (rate < 0) {
        stop('rate must be non-negative')
    }
    x <- list('rate' = rate)
    class(x) <- 'poisson_dist'
    return(x)
}
