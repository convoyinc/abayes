context('Testing the simulation study')

test_that('Test that frequentist simulation works', {
    data_dists <- list('a' = bernoulli_dist(0.4), 'b' = bernoulli_dist(0.5))
    priors <- list('a' = beta_dist(1, 1), 'b' = beta_dist(1, 1))
    result <- investigate_simulations(num_sims = 10
                                      , data_dists = data_dists
                                      , priors = priors
                                      , loss_threshold = 5e-4
                                      , obs_per_round = 10)
    expect_identical(result[['summary_dt']][['avg_loss']], 0)
    
    data_dists <- list('a' = normal_dist(0, 1), 'b' = normal_dist(1, 1))
    priors <- list('a' = normal_gamma_dist(0, 1, 1, 1), 'b' = normal_gamma_dist(0, 1, 1, 1))
    result <- investigate_simulations(num_sims = 10
                                      , data_dists = data_dists
                                      , priors = priors
                                      , loss_threshold = 5e-4
                                      , obs_per_round = 10)
    expect_identical(result[['summary_dt']][['avg_loss']], 0)
    
    data_dists <- list('a' = poisson_dist(1), 'b' = poisson_dist(2))
    priors <- list('a' = gamma_dist(1, 1), 'b' = gamma_dist(1, 1))
    result <- investigate_simulations(num_sims = 10
                                      , data_dists = data_dists
                                      , priors = priors
                                      , loss_threshold = 5e-4
                                      , obs_per_round = 10)
    expect_identical(result[['summary_dt']][['avg_loss']], 0)
})