context('Testing functions that stop the tests')

test_that('single_loss_stop provides the best variant if there is a winner', {
    losses <- c(a = 10, b = 2, c = 30)
    out <- single_loss_stop(losses = losses, loss_threshold = 25)
    expect_equal(out, list(stop = TRUE, variant = 'b'))
})

test_that('single_loss_stop provides NULL if no variant has a small loss', {
    losses <- c(a = 10, b = 2, c = 30)
    out <- single_loss_stop(losses = losses, loss_threshold = 1)
    expect_equal(out, list(stop = FALSE, variant = NULL))
})
