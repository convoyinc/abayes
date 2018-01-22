#' @title Calculate Parameters For Beta Distribution
#' @name calc_beta_dist
#' @description Calculate the parameters for a beta distribution parameterized
#'              by the expected value and standard deviation.
#' @export
#' @param mu The expected value of x (see details)
#' @param sigma The standard deviation of x (see details)
#' @details If x ~ Beta(alpha, beta), then \code{mu} = E[x] and \code{sigma^2} = Var(x)
#' @return An object of class \code{beta_dist}
calc_beta_dist <- function(mu, sigma) {
    alpha <- ((1 - mu) * mu ^ 2 - mu * sigma ^ 2) / sigma ^ 2
    beta <- alpha / mu - alpha
    return(beta_dist('alpha' = alpha, 'beta' = beta))
}

#' @title CDF of Parameterized Beta Distribution
#' @name beta_cdf
#' @description Calculate the area to the left or right of \code{bound} for
#'              a beta distribution parameterized by the expected value and
#'              standard deviation.
#' @export
#' @importFrom stats pbeta
#' @inheritParams calc_beta_dist
#' @param bound The quantile of the distribution of interest
#' @param lower.tail A logical that indicates whether to find the area of the upper or lower tail.
#'                   Default is \code{TRUE}.
#' @details If x ~ Beta(A, B), the \code{mu} = E[x] and \code{sigma^2} = Var(x)
#' @return The area under the desired tail.
beta_cdf <- function(mu, sigma, bound, lower.tail = TRUE) {
    beta_dist <- calc_beta_dist(mu, sigma)
    return(stats::pbeta(bound, beta_dist[['alpha']], beta_dist[['beta']], lower.tail = lower.tail))
}

#' @title Calculate Parameters For Normal Gamma Distribution
#' @name calc_normal_gamma_dist
#' @description Calculate the parameters for a normal gamma distribution parameterized
#'              by the expected value and standard deviation.
#' @export
#' @param mu The expected value of x (see details)
#' @param tau The expected value of T (see details)
#' @param sigma_mu The standard deviation of x (see details)
#' @param sigma_tau The standard deviation of T (see details)
#' @details If (x, T) ~ NormalGamma(mu, lambda, alpha, beta), then \code{mu} = E[x],
#'          \code{tau} = E[T], \code{sigma_mu^2} = Var(x), and \code{sigma_tau^2} = Var(T)
#' @return An object of class \code{normal_gamma_dist}
calc_normal_gamma_dist <- function(mu, tau, sigma_mu, sigma_tau) {
    beta <- tau / (sigma_tau ^ 2)
    alpha <- beta * tau
    lambda <- beta / (sigma_mu ^ 2 * (alpha - 1))
    return(normal_gamma_dist('mu' = mu, 'lambda' = lambda, 'alpha' = alpha, 'beta' = beta))
}

#' @title Calculate Parameters For Gamma Distribution
#' @name calc_gamma_dist
#' @description This function determines the parameters for a gamma distribution
#'              from the desired mean
#' @export
#' @inheritParams calc_beta_dist
#' @details If x ~ Gamma(alpha, beta), then \code{mu} = E[x] and \code{sigma^2} = Var(x)
#' @return A named list
calc_gamma_dist <- function(mu, sigma) {
    beta <- mu / (sigma ^ 2)
    alpha <- beta * mu
    return(gamma_dist('alpha' = alpha, 'beta' = beta))
}

#' @title CDF of Parameterized Gamma Distribution
#' @name gamma_cdf
#' @description Calculate the area to the left or right of \code{bound} for
#'              a gamma distribution parameterized by the expected value and
#'              standard deviation.
#' @export
#' @importFrom stats pgamma
#' @inheritParams calc_beta_dist
#' @inheritParams beta_cdf
#' @details If x ~ Gamma(alpha, beta), then \code{mu} = E[x] and \code{sigma^2} = Var(x)
#' @return The area under the desired tail.
gamma_cdf <- function(mu, sigma, bound, lower.tail = TRUE) {
    gamma_dist <- calc_gamma_dist(mu, sigma)
    return(stats::pgamma(bound, gamma_dist[['alpha']], gamma_dist[['beta']], lower.tail = lower.tail))
}

#' @title find_percentile

#' @title Approximate Distribution Solver
#' @name approx_solver
#' @description Determine the standard deviation of a distribution that will provide
#'              the desired amount of support at a certain bound.
#' @export
#' @inheritParams gamma_cdf
#' @importFrom data.table between
#' @param bound The desired maximum of the distribution
#' @param desired_support The amount of probability mass to be more extreme than \code{bound}
#' @param cdf_fun A CDF
#' @param lower_sigma A guess of the lower bound on the standard deviation
#' @param upper_sigma A guess of the upper bound on the standard deviation
#' @param tolerance Return \code{NULL}, if we cannot find a value that gives support
#'                  within \code{tolerance} of \code{desired_support}
#' @return The optimal standard deviation
approx_solver <- function(mu
                          , bound
                          , desired_support
                          , cdf_fun
                          , lower_sigma
                          , upper_sigma
                          , lower.tail = TRUE
                          , tolerance = 0.01
) {
    
    # get a reasonable range of lower and upper values
    sigma_range <- c(lower_sigma, upper_sigma)
    prob_range <- cdf_fun(mu = mu, sigma = sigma_range, bound = bound, lower.tail = lower.tail)
    
    if (!data.table::between(desired_support, prob_range[1], prob_range[2])) {
        # lower and upper do not contain desired support
        # return(approx_solver(expected, bound, desired_support, cdf_fun, lower.tail
        #        , lower_multipler * 0.5, upper_multiplier * 2, tolerance))
        return(NULL)
    }
    
    candidates <- seq(sigma_range[1], sigma_range[2], length.out = 1000)
    probs <- cdf_fun(mu = mu, sigma = candidates, bound = bound, lower.tail = lower.tail)
    best <- which.min(abs(probs - desired_support))
    if (abs(probs[best] - desired_support) > tolerance) {
        return(NULL)
    } else {
        return(candidates[best])
    }
}

#' @title Get Beta Distribution With Desired Support
#' @name get_supported_beta
#' @description Determines the parameters for a beta distribution given an expected value
#'              and desired support at some bound.
#' @inheritParams calc_beta_dist
#' @inheritParams approx_solver
#' @details If x ~ Beta(alpha, beta), then \code{mu} = E[x] and \code{sigma^2} = Var(x)
#' @export
#' @return An object of class \code{beta_dist}
get_supported_beta <- function(mu, bound, desired_support = 0.05) {
    if (bound <= mu) {
        return('The maximum possible rate must be greater than expected rate.')
    }
    
    if (!all(data.table::between(c(mu, bound), 0, 1, incbounds = FALSE))) {
        return('Expected and Maximum must be between 0 and 1.')
    }
    
    # beta distribution has a maximum standard deviation of mu - mu ^ 2
    sigma_diff <- min(sqrt(mu - mu ^ 2), bound - mu)
    
    sigma <- approx_solver(mu = mu
                           , bound = bound
                           , desired_support = desired_support
                           , cdf_fun = beta_cdf
                           , lower_sigma = 0.1 * sigma_diff
                           , upper_sigma = 0.99 * sigma_diff
                           , lower.tail = FALSE
                           , tolerance = 0.1)
    
    if (is.null(sigma)) {
        return('Please choose less extreme expected and maximum rates')
    } else {
        return(calc_beta_dist(mu = mu, sigma = sigma))
    }
}

#' @title Get Normal Gamma Distribution With Desired Support
#' @name get_supported_normal_gamma
#' @description Determines the parameters for a normal gamma distribution given the expected values
#'              of the distribution and bounds on their maximum values
#' @importFrom stats pnorm rgamma rnorm uniroot
#' @export
#' @inheritParams calc_normal_gamma_dist
#' @inheritParams get_supported_beta
#' @param bound_mu An upper bound on the likely value of x (see details)
#' @param sigma The standard deviation of x (see details)
#' @param bound_sigma The upper bound on the likely value of T (see details)
#' @details If (x, T) ~ NormalGamma(mu, lambda, alpha, beta), then \code{mu} = E[x],
#'          \code{tau} = E[T], \code{sigma_mu^2} = Var(x), and \code{sigma_tau^2} = Var(T)
#' @return An object of class \code{normal_gamma_dist}
get_supported_normal_gamma <- function(mu, bound_mu, sigma, bound_sigma, desired_support = 0.05) {
    if ((bound_mu <= mu) || (bound_sigma <= sigma) || (bound_sigma <= 0)) {
        return('The maximum possible average must be greater than the expected average.')
    }
    
    # first, we need to solve for alpha and beta, then we can use that to solve for lambda
    tau <- 1 / sigma ^ 2
    bound_tau <- 1 / bound_sigma ^ 2
    
    gamma_dist <- get_supported_gamma(mu = tau
                                      , bound = bound_tau
                                      , desired_support = desired_support
                                      , lower.tail = TRUE)
    if (!inherits(gamma_dist, 'gamma_dist')) {
        return('Please choose less extreme difference between expected and maximum.')
    }
    
    sigma_tau <- sqrt(gamma_dist[['alpha']] / gamma_dist[['beta']] ^ 2)
    
    n_samps <- 1e4
    sample_tau <- stats::rgamma(n = n_samps, shape = gamma_dist[['alpha']], rate = gamma_dist[['beta']])
    
    f <- function(sigma_mu) {
        ng_dist <- calc_normal_gamma_dist(mu, tau, sigma_mu, sigma_tau)
        sample_mu <- stats::rnorm(n = n_samps, mean = ng_dist[['mu']], sd = sqrt(1 / (ng_dist[['lambda']] * sample_tau)))
        return(quantile(sample_mu, 1 - desired_support) - bound_mu)
    }
    
    sigma_mu <- tryCatch(stats::uniroot(f, interval = c(0.2, 1) * (bound_mu - mu))$root
                         , error = function(e) NULL)
    if (is.null(sigma_mu)) {
        return('Choose a less extreme difference between the maximum possible average and expected average.')
    }
    ng_dist <- calc_normal_gamma_dist(mu, tau, sigma_mu, sigma_tau)
    
    mu_tail <- mean(stats::pnorm(bound_mu, ng_dist[['mu']], sqrt(1 / (ng_dist[['lambda']] * sample_tau)), lower.tail = FALSE))
    if (abs(mu_tail - desired_support) > 0.1) {
        return('Choose a less extreme difference between the maximum possible average and expected average.')
    }
    
    return(ng_dist)
}

#' @title Get parameters of gamma distribution from expected and maximum rate
#' @name get_supported_gamma
#' @description This function determines the parameters for a gamma distribution
#' @inheritParams calc_gamma_dist
#' @inheritParams get_supported_beta
#' @param lower.tail A boolean that indicates whether we want to get the support on the upper or lower tail
#' @export
#' @details If x ~ Gamma(alpha, beta), then \code{mu} = E[x] and \code{sigma^2} = Var(x)
#' @return A \code{gamma_dist} object
get_supported_gamma <- function(mu, bound, desired_support = 0.05, lower.tail = FALSE) {
    if (!lower.tail) {
        if ((bound <= mu) || (bound <= 0)) {
            return('The maximum must be greater than the expected and positive.')
        }
        sigma_diff <- bound - mu
    } else {
        sigma_diff <- mu - bound
    }
    
    sigma <- approx_solver(mu = mu
                           , bound = bound
                           , desired_support = desired_support
                           , cdf_fun = gamma_cdf
                           , lower_sigma = sigma_diff * 0.1
                           , upper_sigma = sigma_diff * 0.99
                           , lower.tail = lower.tail
                           , tolerance = 0.1)
    
    if (is.null(sigma)) {
        return('Please choose less extreme difference between expected and maximum.')
    } else {
        return(calc_gamma_dist(mu, sigma))
    }
}

