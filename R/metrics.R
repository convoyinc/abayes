use_exact_beta <- function(dist_a, dist_b, cutoff = 1000) {
    is_small_beta <- function(dist, cutoff) {
        return(dist[['alpha']] + dist[['beta']] < cutoff)
    }
    return(is_small_beta(dist_a, cutoff) || is_small_beta(dist_b, cutoff))
}

use_exact_gamma <- function(dist_a, dist_b, cutoff = 1000) {
    is_small_gamma <- function(dist, cutoff) {
        return(dist[['alpha']] < cutoff)
    }
    return(is_small_gamma(dist_a, cutoff) || is_small_gamma(dist_b, cutoff))
}

#' @export
#' @importFrom stats pnorm
b_gt_a.beta_dist <- function(dist_a, dist_b, theta_a, theta_b, exact = NULL, ...) {
    alpha_a <- dist_a[['alpha']]
    beta_a <- dist_a[['beta']]
    alpha_b <- dist_b[['alpha']]
    beta_b <- dist_b[['beta']]
    
    if (is.null(exact)) {
        exact <- use_exact_beta(dist_a, dist_b, ...)
    }
    
    if (exact) {
        iter <- seq(0, round(alpha_b) - 1)
        alpha_a <- round(alpha_a)
        w <- lbeta(alpha_a + iter, beta_a + beta_b)
        x <- log(beta_b + iter)
        y <- lbeta(1 + iter, beta_b)
        z <- lbeta(alpha_a, beta_a)
        return(sum(exp(w - x - y - z)))
    } else {
        mu_a <- alpha_a / (alpha_a + beta_a)
        mu_b <- alpha_b / (alpha_b + beta_b)
        var_a <- alpha_a * beta_a / ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
        var_b <- alpha_b * beta_b / ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
        return(stats::pnorm(0, mu_b - mu_a, sqrt(var_a + var_b), lower.tail = FALSE))
    }
}

#' @export
#' @importFrom stats pnorm
b_gt_a.normal_gamma_dist <- function(dist_a, dist_b, theta_a, theta_b, exact = NULL, ...) {
    return(mean(theta_b > theta_a))
}

#' @export
#' @importFrom stats pnorm
b_gt_a.gamma_dist <- function(dist_a, dist_b, theta_a, theta_b, exact = NULL, ...) {
    alpha_a <- dist_a[['alpha']]
    beta_a <- dist_a[['beta']]
    alpha_b <- dist_b[['alpha']]
    beta_b <- dist_b[['beta']]
    
    if (is.null(exact)) {
        exact <- use_exact_gamma(dist_a, dist_b)
    }
    
    if (exact) {
        iter <- seq(0, round(alpha_b) - 1)
        alpha_a <- round(alpha_a)
        x1 <- iter * log(beta_b)
        x2 <- alpha_a * log(beta_a)
        x3 <- log(iter + alpha_a)
        x4 <- lbeta(iter + 1, alpha_a)
        x5 <- (iter + alpha_a) * log(beta_a + beta_b)
        return(sum(exp(x1 + x2 - (x3 + x4 + x5))))
    } else {
        mu_a <- alpha_a / beta_a
        mu_b <- alpha_b / beta_b
        var_a <- alpha_a / beta_a ^ 2
        var_b <- alpha_b / beta_b ^ 2
        return(stats::pnorm(0, mu_b - mu_a, sqrt(var_a + var_b), lower.tail = FALSE))
    }
}

#' @title Probability Variant B is Greater Than Variant A
#' @name b_gt_a
#' @description Given two distributions, find the probability that the expected
#'              value of variant B is greater than the expected value of variant A
#' @param dist_a Some distribution object (see examples)
#' @param dist_b Some distribution object (see examples)
#' @param theta_a A vector of simulated values from \code{dist_a}
#' @param theta_b A vector of simulated values from \code{dist_b}
#' @param exact A boolean that indicates whether the calculation should be approximated
#'              using the normal distribution. Default is \code{NULL}, which means
#'              that it will use the normal distribution if there is sufficient data.
#' @param ... Arguments to be passed onto other methods
#' @export
#' @return A numeric value
b_gt_a <- function(dist_a, dist_b, theta_a, theta_b, exact = NULL, ...) {
    UseMethod('b_gt_a')
}

#' @export
expected_loss_b.beta_dist <- function(dist_a
                                      , dist_b
                                      , theta_a
                                      , theta_b
                                      , method = c('absolute', 'percent')
                                      , ...
) {
    method <- match.arg(method)
    
    alpha_a <- dist_a[['alpha']]
    beta_a <- dist_a[['beta']]
    alpha_b <- dist_b[['alpha']]
    beta_b <- dist_b[['beta']]
    
    if (method == 'absolute') {
        x1 <- lbeta(alpha_a + 1, beta_a)
        y1 <- log(1 - b_gt_a(beta_dist(alpha_a + 1, beta_a), dist_b, ...))
        z1 <- lbeta(alpha_a, beta_a)
        
        x2 <- lbeta(alpha_b + 1, beta_b)
        y2 <- log(1 - b_gt_a(dist_a, beta_dist(alpha_b + 1, beta_b), ...))
        z2 <- lbeta(alpha_b, beta_b)
        
        return(exp(x1 + y1 - z1) - exp(x2 + y2 - z2))
    } else {
        prob_1 <- 1 - b_gt_a(dist_a, dist_b, ...)
        
        x <- lbeta(alpha_a - 1, beta_a)
        y <- lbeta(alpha_a, beta_a)
        z <- lbeta(alpha_b + 1, beta_b)
        w <- lbeta(alpha_b, beta_b)
        prob_2 <- log(1 - b_gt_a(beta_dist(alpha_a - 1, beta_a)
                                 , beta_dist(alpha_b + 1, beta_b), ...))
        return(prob_1 - exp(x - y + z - w + prob_2))
    }
}

#' @export
expected_loss_b.normal_gamma_dist <- function(dist_a
                                              , dist_b
                                              , theta_a
                                              , theta_b
                                              , method = c('absolute', 'percent')
                                              , ...
) {
    return(mean(pmax(theta_a - theta_b, 0)))
}

#' @export
expected_loss_b.gamma_dist <- function(dist_a
                                       , dist_b
                                       , theta_a
                                       , theta_b
                                       , method = c('absolute', 'percent')
                                       , ...
) {
    alpha_a <- dist_a[['alpha']]
    beta_a <- dist_a[['beta']]
    alpha_b <- dist_b[['alpha']]
    beta_b <- dist_b[['beta']]
    
    x1 <- lgamma(alpha_a + 1)
    y1 <- log(1 - b_gt_a(gamma_dist(alpha_a + 1, beta_a), dist_b))
    z1 <- log(beta_a)
    w1 <- lgamma(alpha_a)
    
    x2 <- lgamma(alpha_b + 1)
    y2 <- log(1 - b_gt_a(dist_a, gamma_dist(alpha_b + 1, beta_b)))
    z2 <- log(beta_b)
    w2 <- lgamma(alpha_b)
    return(exp(x1 + y1 - z1 - w1) - exp(x2 + y2 - z2 - w2))
}

#' @title Expected Loss of Choosing Variant B
#' @name expected_loss_b
#' @description This function calculates the expected loss of choosing variant B
#' @export
#' @param dist_a The distribution object for variant A
#' @param dist_b The distribution object for variant B
#' @param theta_a A vector of simulated values from \code{dist_a}
#' @param theta_b A vector of simulated values from \code{dist_b}
#' @param method One of \code{'absolute'} or \code{'percent'} that indicates
#'               whether the loss function takes the absolute difference
#'               or the percent difference between \code{theta_a} and \code{theta_b}
#' @param ... Arguments to be passed onto other methods
#' @return A numeric
expected_loss_b <- function(dist_a
                            , dist_b
                            , theta_a
                            , theta_b
                            , method = c('absolute', 'percent')
                            , ...
) {
    UseMethod('expected_loss_b')
}

#' @title Expected Losses For Variants
#' @name get_losses
#' @description Evaluate the expected loss for each variant
#' @export
#' @param posteriors A list of distribution objects that identify the posterior distributions
#'                   for each variant
#' @inheritParams ab_arguments
#' @inheritParams expected_loss_b
#' @importFrom purrr map
#' @inheritParams ab_arguments
#' @return A list of expected losses for each variant
get_losses <- function(posteriors, sim_batch_size, method = c('absolute', 'percent')) {
    dist_name <- class(posteriors[['a']])
    dist_a <- posteriors[['a']]
    dist_b <- posteriors[['b']]
    if (dist_name %in% c('normal_gamma_dist')) {
        thetas <- purrr::map(posteriors, function(dist) simulate_data(dist = dist, n = sim_batch_size))
    } else {
        thetas <- NULL
    }
    
    loss_a <- expected_loss_b(dist_a = dist_b, dist_b = dist_a
                              , theta_a = thetas[['b']], theta_b = thetas[['a']]
                              , method = method)
    loss_b <- expected_loss_b(dist_a = dist_a, dist_b = dist_b
                              , theta_a = thetas[['a']], theta_b = thetas[['b']]
                              , method = method)
    
    return(list(a = loss_a, b = loss_b))
}

#' @title Get Bayesian A/B Testing Metrics
#' @name get_metrics
#' @description Calculate various metrics
#' @export
#' @importFrom purrr map
#' @param posteriors A list of distribution objects
#' @param sim_batch_size How many observations of data to simulate
#' @param method What type of loss to calculate?
#' @return A named list with the metrics
get_metrics <- function(posteriors, sim_batch_size, method = c('absolute', 'percent')) {
    dist_a <- posteriors[['a']]
    dist_b <- posteriors[['b']]
    
    # simulate data for effect size credible interval and normal gamma
    thetas <- purrr::map(posteriors, function(dist) simulate_data(dist = dist, n = sim_batch_size))
    
    # probability that one metric is greater than another
    prob_b_gt_a <- b_gt_a(dist_a = dist_a
                          , dist_b = dist_b
                          , theta_a = thetas[['a']]
                          , theta_b = thetas[['b']])
    
    # risk for each variant
    loss_a <- expected_loss_b(dist_a = dist_b
                              , dist_b = dist_a
                              , theta_a = thetas[['b']]
                              , theta_b = thetas[['a']]
                              , method = method)
    loss_b <- expected_loss_b(dist_a = dist_a
                              , dist_b = dist_b
                              , theta_a = thetas[['a']]
                              , theta_b = thetas[['b']]
                              , method = method)
    
    # effect size for variant b
    effect <- sim_effect_size(theta_a = thetas[['a']], theta_b = thetas[['b']])
    
    return(list('loss_a' = loss_a, 'loss_b' = loss_b, 'prob_b_gt_a' = prob_b_gt_a
                , 'effect_lower' = effect[['lower']], 'effect_expected' = effect[['avg']]
                , 'effect_upper' = effect[['upper']]))
}

#' @title Actual Loss of an Experiment
#' @name get_actual_loss
#' @description Once the experiment has concluded, measure the loss of the decision
#' @inheritParams ab_arguments
#' @param selected_variant A string that identifies the winning variant.
#' @importFrom purrr map
#' @export
#' @return Numeric
get_actual_loss <- function(data_dists, selected_variant) {
    dist_name <- class(data_dists[['a']])
    metric_mapping <- list(bernoulli_dist = 'rate'
                           , normal_dist = 'mu'
                           , poisson_dist = 'rate')
    truth <- purrr::map(data_dists, metric_mapping[[dist_name]])
    if (selected_variant == 'a') {
        return(pmax(truth[['b']] - truth[['a']], 0))
    } else {
        return(pmax(truth[['a']] - truth[['b']], 0))
    }
}

#' @title Simulate the effect size of variant B - variant A
#' @name sim_effect_size
#' @description This function simulates the effect size (B - A)
#' @export
#' @importFrom stats quantile
#' @param theta_a A vector of draws from the posterior of a
#' @param theta_b A vector of draws from the posterior of b
#' @return A list containing the 2.5%, mean, and 97.5% of the effect size
sim_effect_size <- function(theta_a, theta_b) {
    diffs <- theta_b - theta_a
    credible_interval <- stats::quantile(diffs, c(0.025, 0.975))
    return(list('lower' = credible_interval['2.5%']
                , 'avg' = mean(diffs)
                , 'upper' = credible_interval['97.5%']))
}




