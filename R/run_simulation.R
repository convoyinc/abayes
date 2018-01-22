#' @title Simulate a Bayesian A/B Test
#' @name simulate_ab_test
#' @description Given true data generating distributions and prior distributions for
#'              variants A and B, simulate the data, calculate the necessary statistics
#'              and declare one of the tests a winner.
#' @inheritParams ab_arguments
#' @export
#' @importFrom purrr map
#' @importFrom data.table set
#' @details In order to create \code{data_dists} and \code{priors}, you need to use the following
#'          for data generating distributions: \code{\link{bernoulli_dist}},
#'          \code{\link{normal_dist}}, \code{\link{poisson_dist}} and the following for prior
#'          distributions \code{\link{beta_dist}}, \code{\link{normal_gamma_dist}},
#'          \code{\link{beta_dist}},
#' @seealso \code{\link{bernoulli_dist}} \code{\link{normal_dist}} \code{\link{poisson_dist}}
#'          \code{\link{beta_dist}} \code{\link{normal_gamma_dist}} \code{\link{beta_dist}}
#' @return A list that contains the name of the winning variant, the number of observations used,
#'         the loss of the decision, whether the test finished, the metrics from each round, and
#'         the raw data.
simulate_ab_test <- function(data_dists
                             , priors
                             , loss_threshold
                             , obs_per_round = 1000
                             , max_rounds = 100
                             , sim_batch_size = 1e5
) {
    if (any(c(loss_threshold, obs_per_round, max_rounds, sim_batch_size) <= 0)) {
        stop('loss_threshold, obs_per_round, max_rounds, and sim_batch_size must be positive')
    }
    validate_inputs(data_dists, priors, obs_per_round)
    variants <- names(data_dists)
    num_variants <- length(data_dists)
    obs_per_variant <- obs_per_round / num_variants
    
    evidence_dt <- create_empty_dt(num_rows = max_rounds * obs_per_variant, column_names = variants)
    loss_dt <- create_empty_dt(num_rows = max_rounds, column_names = variants)
    best_variant <- NULL
    
    for (round_num in 1:max_rounds) {
        
        # simulate new set of data
        which_rows <- (1 + (round_num - 1) * obs_per_variant):(round_num * obs_per_variant)
        new_evidence <- purrr::map(data_dists, function(dist) simulate_data(dist = dist, n = obs_per_variant))
        data.table::set(evidence_dt, i = which_rows, j = names(evidence_dt), value = new_evidence)
        
        # update priors with new evidence
        posteriors <- update_priors(priors = priors
                                    , evidence_dt = evidence_dt[1:max(which_rows)])
        
        # evaluate metrics
        losses <- get_losses(posteriors = posteriors
                             , sim_batch_size = sim_batch_size)
        
        data.table::set(loss_dt, i = round_num, j = names(loss_dt), value = losses)

        # determine if we can stop the experiment
        decision <- single_loss_stop(losses = unlist(losses)
                                     , loss_threshold = loss_threshold)
        if (decision[['stop']]) {
            best_variant <- decision[['variant']]
            break
        }
    }
    test_finished <- as.numeric(!is.null(best_variant))
    if (test_finished) {
        actual_loss <- get_actual_loss(data_dists = data_dists
                                       , selected_variant = best_variant)
    } else {
        actual_loss <- NA_real_
    }
    
    
    out <- list(best_variant = best_variant
                , num_obs = obs_per_round * round_num
                , actual_loss = actual_loss
                , test_finished = test_finished
                , loss_dt = loss_dt[1:round_num]
                , evidence_dt = evidence_dt[1:(round_num * obs_per_variant)])
    return(out)
}

#' @title Validate Inputs to Bayesian A/B Simulation
#' @name validate_inputs
#' @description Check the validity of the parameters being used in the simulation.
#' @export
#' @inheritParams ab_arguments
#' @return NULL if all of the tests pass. Else, it will fail loudly.
validate_inputs <- function(data_dists
                            , priors
                            , obs_per_round
) {
    # check the number and names of the variants
    num_variants <- length(data_dists)
    if (num_variants != 2) {
        stop('Currently, only A/B tests with two variants are supported')
    }
    true_names <- names(data_dists)
    prior_names <- names(priors)
    if (is.null(true_names) || !identical(true_names, prior_names) || !identical(true_names, letters[1:num_variants])) {
        stop('data_dists and priors must be named lists. And the elements must be named "a" and "b"')
    }
    
    supported_data_dists <- c('bernoulli_dist', 'normal_dist', 'poisson_dist')
    supported_priors <- c('beta_dist', 'normal_gamma_dist', 'gamma_dist')
    data_a <- class(data_dists[['a']])
    data_b <- class(data_dists[['b']])
    prior_a <- class(priors[['a']])
    prior_b <- class(priors[['b']])
    
    if (!identical(data_a, data_b) | !(data_a %in% supported_data_dists)) {
        stop(paste('All data distributions need to be identical. Supported:'
                   , paste(supported_data_dists, collapse = ', ')))
    }
    
    if (!identical(prior_a, prior_b) | !(prior_a %in% supported_priors)) {
        stop(paste('All prior distributions need to be identical. Supported:'
                   , paste(supported_priors, collapse = ', ')))
    }
    
    # check that the number of obs to sample each round is divisible by the number of variants
    if (obs_per_round %% num_variants != 0) {
        stop('Number of obs must be divisible by number of variants')
    }
    
    return(NULL)
}

