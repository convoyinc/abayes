context('Testing functions that construct priors')

test_that('calc_beta_dist can get a beta distribution from mu and sigma', {
    mu <- 0.5
    sigma <- 0.01
    dist <- calc_beta_dist(mu = mu, sigma = sigma)
    expected <- beta_dist(alpha = 1249.5, beta = 1249.5)
    expect_identical(dist, expected)
})

test_that('beta_cdf calculates the correct integral', {
    mu <- 0.5
    sigma <- 0.01
    bound <- 0.51
    lower.tail <- FALSE
    area <- beta_cdf(mu = mu, sigma = sigma, bound = bound, lower.tail = lower.tail)
    expected <- pbeta(0.51, 1249.5, 1249.5, lower.tail = FALSE)
    expect_identical(area, expected)
})

test_that('calc_normal_gamma_dist can get a normal gamma distribution', {
    mu <- 10
    tau <- 2
    sigma_mu <- 2
    sigma_tau <- 0.5
    dist <- calc_normal_gamma_dist(mu = mu, tau = tau, sigma_mu = sigma_mu, sigma_tau = sigma_tau)
    expected <- normal_gamma_dist(mu = 10, lambda = 2/15, alpha = 16, beta = 8)
    expect_identical(dist, expected)
})

test_that('calc_gamma_dist can get a gamma distribution from expected value and standard deviation', {
    mu <- 10
    sigma <- 5
    dist <- calc_gamma_dist(mu = mu, sigma = sigma)
    expected <- gamma_dist(alpha = 4, beta = 0.4)
    expect_identical(dist, expected)
})

test_that('gamma_cdf calculates the correct integral', {
    mu <- 3
    sigma <- 2
    bound <- 4
    lower.tail <- FALSE
    area <- gamma_cdf(mu = mu, sigma = sigma, bound = bound, lower.tail = lower.tail)
    expected <- pgamma(4, 2.25, 0.75, lower.tail = FALSE)
    expect_identical(area, expected)
})

test_that('get_supported_beta returns correct distribution', {
    run_test <- function(mu, bound, desired_support) {
        beta_dist <- get_supported_beta(mu = mu, bound = bound, desired_support = desired_support)
        expect_true(inherits(beta_dist, 'beta_dist'))
        actual_support <- pbeta(bound, beta_dist[['alpha']], beta_dist[['beta']], lower.tail = FALSE)
        expect_true(abs(actual_support - desired_support) < 0.01)
    }
    
    run_test(mu = 0.1, bound = 0.2, desired_support = 0.05)
    run_test(mu = 0.01, bound = 0.011, desired_support = 0.05)
    run_test(mu = 0.3, bound = 0.5, desired_support = 0.05)
})

test_that('get_supported_normal_gamma returns correct distribution', {
    run_test <- function(mu, bound_mu, sigma, bound_sigma, desired_support) {
        ng_dist <- get_supported_normal_gamma(mu = mu, bound_mu = bound_mu
                                              , sigma = sigma, bound_sigma = bound_sigma
                                              , desired_support = desired_support)
        expect_true(inherits(ng_dist, 'normal_gamma_dist'))
        sample_tau <- rgamma(n = 1e4, shape = ng_dist[['alpha']], rate = ng_dist[['beta']])
        actual_support <- mean(pnorm(bound_mu
                                     , ng_dist[['mu']]
                                     , sqrt(1 / (ng_dist[['lambda']] * sample_tau))
                                     , lower.tail = FALSE))
        expect_true(abs(actual_support - desired_support) < 0.01)
    }
    
    run_test(mu = 0, bound_mu = 2, sigma = 2, bound_sigma = 4, desired_support = 0.05)
    run_test(mu = 10, bound_mu = 25, sigma = 100, bound_sigma = 150, desired_support = 0.05)
})

test_that('get_supported_gamma returns correct distribution', {
    run_test <- function(mu, bound, desired_support) {
        gamma_dist <- get_supported_gamma(mu = mu, bound = bound, desired_support = desired_support)
        expect_true(inherits(gamma_dist, 'gamma_dist'))
        actual_support <- pgamma(bound, gamma_dist[['alpha']], gamma_dist[['beta']], lower.tail = FALSE)
        expect_true(abs(actual_support - desired_support) < 0.01)
    }
    
    run_test(mu = 10, bound = 30, desired_support = 0.05)
    run_test(mu = 25, bound = 30, desired_support = 0.05)
})

