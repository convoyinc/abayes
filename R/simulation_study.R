#' @title Investigate Properties of Bayesian A/B Testing
#' @name investigate_simulations
#' @description Run multiple simulations of an A/B test in order to evaluate various properties
#' @export
#' @inheritParams ab_arguments
#' @param num_sims A positive integer that specifies how many simulations to perform.
#' @param sampling_distribution An list of distribution objects that specifies how the data generating
#'                              distributions should be created.
#' @param num_cores How many cores to use in the parallelization of tests. Default is \code{NULL}, which
#'                  means no parallelization.
#' @importFrom data.table setnames set rbindlist
#' @importFrom purrr map_dbl
#' @importFrom parallel makeCluster clusterExport clusterApplyLB stopCluster
#' @return A list containing two data.tables: one with summary statistics for each simulation
#'         and one with the averages over all of the simulations.
investigate_simulations <- function(num_sims
                                    , priors
                                    , loss_threshold
                                    , data_dists = NULL
                                    , sampling_distribution = NULL
                                    , obs_per_round = 1000
                                    , max_rounds = 100
                                    , sim_batch_size = 1e5
                                    , num_cores = NULL
) {
    
    if (!is.null(sampling_distribution)) {
        distributions <- get_data_dists(dist = sampling_distribution
                                        , n = num_sims
                                        , num_variants = 2)
    } else {
        distributions <- rep(list(data_dists), num_sims)
    }
    
    if (!is.null(num_cores)) {
        cluster <- parallel::makeCluster(num_cores)
        parallel::clusterExport(cluster
                                , varlist = c('simulate_ab_test', 'priors'
                                              , 'loss_threshold', 'obs_per_round'
                                              , 'max_rounds', 'sim_batch_size')
                                , envir = environment())
        result_list <- parallel::clusterApplyLB(
            cluster
            , x = distributions
            , fun = function(dist) {
                result <- simulate_ab_test(data_dists = dist
                                           , priors = priors
                                           , loss_threshold = loss_threshold
                                           , obs_per_round = obs_per_round
                                           , max_rounds = max_rounds
                                           , sim_batch_size = sim_batch_size)
                return(list(num_obs_seen = result[['num_obs']]
                            , test_finished = result[['test_finished']]
                            , bad_decision = as.numeric(result[['actual_loss']] > 0)
                            , loss = result[['actual_loss']]))
            })
        parallel::stopCluster(cluster)
        sim_dt <- data.table::rbindlist(result_list)
    } else {
        sim_columns <- c('num_obs_seen', 'test_finished', 'bad_decision', 'loss')
        sim_dt <- create_empty_dt(num_rows = num_sims, column_names = sim_columns)
        for (i in 1:num_sims) {
            result <- simulate_ab_test(data_dists = distributions[[i]]
                                       , priors = priors
                                       , loss_threshold = loss_threshold
                                       , obs_per_round = obs_per_round
                                       , max_rounds = max_rounds
                                       , sim_batch_size = sim_batch_size)
            result_list <- list(result[['num_obs']]
                                , result[['test_finished']]
                                , as.numeric(result[['actual_loss']] > 0)
                                , result[['actual_loss']])
            
            data.table::set(sim_dt, i = i, j = names(sim_dt), value = result_list)
        }
    }
    summary_dt <- sim_dt[, .(avg_obs_seen = mean(num_obs_seen)
                             , p80_obs_seen = quantile(num_obs_seen, 0.8)
                             , max_obs_seen = max(num_obs_seen)
                             , test_finish_rate = mean(test_finished)
                             , wrong_decision_rate = mean(bad_decision)
                             , avg_loss = mean(loss))]
    
    return(list(sim_dt = sim_dt, summary_dt = summary_dt))
}
