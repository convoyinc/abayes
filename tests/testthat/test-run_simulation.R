context('Test that we can run a whole simulation')

test_that('A simulation is successful with beta', {
    
    data_dists <- list(a = bernoulli_dist(rate = 0.3), b = bernoulli_dist(rate = 0.8))
    priors <- list(a = beta_dist(alpha = 1, beta = 1), b = beta_dist(alpha = 1, beta = 1))
    
    result <- simulate_ab_test(data_dists = data_dists
                               , priors = priors
                               , loss_threshold = 1e-4
                               , obs_per_round = 10
                               , max_rounds = 100)
    expect_equal(result[['best_variant']], 'b')
})

test_that('A simulation is successful with normal gamma', {
    
    data_dists <- list(a = normal_dist(mu = 2, sigma = 1), b = normal_dist(mu = 1, sigma = 2))
    priors <- list(a = normal_gamma_dist(mu = 0, lambda = 1, alpha = 1, beta = 1)
                   , b = normal_gamma_dist(mu = 0, lambda = 1, alpha = 1, beta = 1))
    
    result <- simulate_ab_test(data_dists = data_dists
                               , priors = priors
                               , loss_threshold = 1e-4
                               , obs_per_round = 10
                               , max_rounds = 100)
    expect_equal(result[['best_variant']], 'a')
})

test_that('A simulation is successful with gamma', {
    
    data_dists <- list(a = poisson_dist(rate = 1), b = poisson_dist(rate = 2))
    priors <- list(a = gamma_dist(alpha = 1, beta = 1), b = gamma_dist(alpha = 1, beta = 1))
    
    result <- simulate_ab_test(data_dists = data_dists
                               , priors = priors
                               , loss_threshold = 1e-4
                               , obs_per_round = 10
                               , max_rounds = 100)
    expect_equal(result[['best_variant']], 'b')
})
