context('Testing functions that update prior distributions')

test_that('update_prior.beta_dist fails on bad inputs', {
    prior <- beta_dist(alpha = 5, beta = 10)
    expect_error(update_prior(prior = prior, evidence = 1:3), regexp = 'must be a binary vector')
    expect_error(update_prior(prior = prior), regexp = 'cannot be NULL')
})

test_that('update_prior.beta_dist produces proper output', {
    prior <- beta_dist(alpha = 5, beta = 10)
    post <- update_prior(prior = prior, evidence = c(1, 1, 0, 0))
    expected <- beta_dist(alpha = 7, beta = 12)
    expect_equal(post, expected)
    post <- update_prior(prior = prior, stats = list('num_obs' = 10, 'observed_rate' = 0.5))
    expected <- beta_dist(alpha = 10, beta = 15)
    expect_equal(post, expected)
})

test_that('update_prior.normal_gamma_dist fails on bad inputs', {
    prior <- normal_gamma_dist(mu = 0, lambda = 1, alpha = 5, beta = 10)
    expect_error(update_prior(prior = prior, evidence = c('a', 'b')), regexp = 'must be a numeric vector')
    expect_error(update_prior(prior = prior), regexp = 'cannot be NULL')
})

test_that('update_prior.normal_gamma_dist produces proper output', {
    prior <- normal_gamma_dist(mu = 0, lambda = 1, alpha = 5, beta = 10)
    post <- update_prior(prior = prior, evidence = c(1, 2, 3, 4))
    expected <- normal_gamma_dist(mu = 2, lambda = 5, alpha = 7, beta = 15)
    expect_equal(post, expected)
    post <- update_prior(prior = prior, stats = list('num_obs' = 4, 'avg' = 2.5, 'std_dev' = sd(c(1, 2, 3, 4))))
    expect_equal(post, expected)
})

test_that('update_prior.gamma_dist fails on bad inputs', {
    prior <- gamma_dist(alpha = 5, beta = 10)
    expect_error(update_prior(prior = prior, evidence = -1:3), regexp = 'positive integer vector')
    expect_error(update_prior(prior = prior), regexp = 'cannot be NULL')
})

test_that('update_prior.gamma_dist produces proper output', {
    prior <- gamma_dist(alpha = 5, beta = 10)
    post <- update_prior(prior = prior, evidence = 1:4)
    expected <- gamma_dist(alpha = 15, beta = 14)
    expect_equal(post, expected)
    post <- update_prior(prior = prior, stats = list('num_sessions' = 3, 'observed_count' = 30))
    expected <- gamma_dist(alpha = 35, beta = 13)
    expect_equal(post, expected)
})

test_that('validate_dt fails where it should', {
    df <- data.frame(x = 1:4, y = 2:5)
    expect_error(validate_dt(df), regexp = 'data.table')
    dt <- data.table::data.table(x = 1:4, y = 2:5)
    expect_error(validate_dt(dt, c('x', 'y', 'z')), 'z')
    expect_null(validate_dt(dt, c('x', 'y')))
})

test_that('update priors can update a list of priors', {
    priors <- list('a' = beta_dist(alpha = 1, beta = 1)
                   , 'b' = beta_dist(alpha = 1, beta = 1))
    evidence_dt <- data.table::data.table(a = c(1, 1, 1, 0, 0, 0)
                                          , b = c(1, 1, 1, 1, 0, 0))
    posteriors <- update_priors(priors = priors, evidence_dt = evidence_dt)
    expected <- list('a' = beta_dist(alpha = 4, beta = 4)
                     , 'b' = beta_dist(alpha = 5, beta = 3))
    expect_equal(posteriors, expected)
    stats_dt <- data.table::data.table(variant = c('a', 'b')
                                       , num_obs = c(6, 6)
                                       , observed_rate = c(1 / 2, 2 / 3))
    posteriors <- update_priors(priors = priors, evidence_dt = evidence_dt)
    expect_equal(posteriors, expected)
})


