context('Testing functions that create distribution objects')

test_that('beta_dist fails on bad inputs', {
    expect_error(beta_dist(alpha = 'a', beta = 0), regexp = 'must be numeric')
    expect_error(beta_dist(alpha = -1, beta = 0), regexp = 'must be non-negative')
})

test_that('beta_dist creates a valid object', {
    dist <- beta_dist(alpha = 1, beta = 2)
    expect_equal(class(dist), 'beta_dist')
    expect_equal(names(dist), c('alpha', 'beta'))
    expect_equal(dist[['alpha']], 1)
    expect_equal(dist[['beta']], 2)
})

test_that('normal_gamma_dist fails on bad inputs', {
    expect_error(normal_gamma_dist(mu = 'a', lambda = 0, alpha = 1, beta = 2)
                 , regexp = 'must be numeric')
    expect_error(normal_gamma_dist(mu = -1, lambda = -1, alpha = 1, beta = 2)
                 , regexp = 'must be non-negative')
})

test_that('normal_gamma_dist creates a valid object', {
    dist <- normal_gamma_dist(mu = 0, lambda = 3, alpha = 1, beta = 2)
    expect_equal(class(dist), 'normal_gamma_dist')
    expect_equal(names(dist), c('mu', 'lambda', 'alpha', 'beta'))
    expect_equal(dist[['mu']], 0)
    expect_equal(dist[['lambda']], 3)
    expect_equal(dist[['alpha']], 1)
    expect_equal(dist[['beta']], 2)
})

test_that('gamma_dist fails on bad inputs', {
    expect_error(gamma_dist(alpha = 'a', beta = 0), regexp = 'must be numeric')
    expect_error(gamma_dist(alpha = -1, beta = 0), regexp = 'must be non-negative')
})

test_that('gamma_dist creates a valid object', {
    dist <- gamma_dist(alpha = 1, beta = 2)
    expect_equal(class(dist), 'gamma_dist')
    expect_equal(names(dist), c('alpha', 'beta'))
    expect_equal(dist[['alpha']], 1)
    expect_equal(dist[['beta']], 2)
})

test_that('bernoulli_dist fails on bad inputs', {
    expect_error(bernoulli_dist(rate = 'a'), regexp = 'must be numeric')
    expect_error(bernoulli_dist(rate = 1.5), regexp = 'must be between 0 and 1')
})

test_that('bernoulli_dist creates a valid object', {
    dist <- bernoulli_dist(rate = 0.5)
    expect_equal(class(dist), 'bernoulli_dist')
    expect_equal(names(dist), 'rate')
    expect_equal(dist[['rate']], 0.5)
})

test_that('normal_dist fails on bad inputs', {
    expect_error(normal_dist(mu = 'a', sigma = 1), regexp = 'must be numeric')
    expect_error(normal_dist(mu = 1.5, sigma = -1), regexp = 'must be non-negative')
})

test_that('normal_dist creates a valid object', {
    dist <- normal_dist(mu = 0, sigma = 1)
    expect_equal(class(dist), 'normal_dist')
    expect_equal(names(dist), c('mu', 'sigma'))
    expect_equal(dist[['mu']], 0)
    expect_equal(dist[['sigma']], 1)
})

test_that('poisson_dist fails on bad inputs', {
    expect_error(poisson_dist(rate = 'a'), regexp = 'must be numeric')
    expect_error(poisson_dist(rate = -1), regexp = 'must be non-negative')
})

test_that('poisson_dist creates a valid object', {
    dist <- poisson_dist(rate = 5)
    expect_equal(class(dist), 'poisson_dist')
    expect_equal(names(dist), 'rate')
    expect_equal(dist[['rate']], 5)
})

