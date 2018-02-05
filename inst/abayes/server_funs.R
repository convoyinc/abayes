library(data.table)

MAX_VAL <- 1e6
MAX_OBS <- 1e8

create_prior_dist <- function(dist_name
                              , expected_rate
                              , upper_bound_rate
                              , expected_mean
                              , upper_bound_mean
                              , expected_sd
                              , upper_bound_sd
                              , expected_lambda
                              , upper_bound_lambda
) {
    if (dist_name == 'binomial') {
        in_vals <- c(expected_rate, upper_bound_rate)
        if (any(is.na(in_vals))) {
            return('Please enter data')
        }
        if (!all(data.table::between(in_vals, 0, 1))) {
            return('Rates must be between 0 and 1')
        }
        return(get_supported_beta(mu = expected_rate, bound = upper_bound_rate, desired_support = 0.05))
    } else if (dist_name == 'normal') {
        means <- c(expected_mean, upper_bound_mean)
        sds <- c(expected_sd, upper_bound_sd)
        if (any(is.na(c(means, sds)))) {
            return('Please enter data')
        }
        if (!all(data.table::between(means, -MAX_VAL, MAX_VAL))) {
            return('Average must be less extreme')
        }
        if (!all(data.table::between(sds, 0, MAX_VAL, incbounds = FALSE))) {
            return('Standard deviations must be less extreme')
        }
        return(get_supported_normal_gamma(mu = expected_mean, bound_mu = upper_bound_mean
                                          , sigma = expected_sd, bound_sigma = upper_bound_sd
                                          , desired_support = 0.05))
    } else {
        in_vals <- c(expected_lambda, upper_bound_lambda)
        if (any(is.na(in_vals))) {
            return('Please enter data')
        }
        if (!all(data.table::between(in_vals, 0, MAX_VAL))) {
            return('Values must be less extreme')
        }
        return(get_supported_gamma(mu = expected_lambda, bound = upper_bound_lambda, desired_support = 0.05))
    }
}

create_results_dt <- function(dist_name
                              , obs_a, obs_b, rate_a, rate_b
                              , count_a, count_b, mean_a, mean_b, sd_a, sd_b
                              , num_a, num_b, total_a, total_b
) {
    if (dist_name == 'binomial') {
        obs <- c(obs_a, obs_b)
        rates <- c(rate_a, rate_b)
        if (any(is.na(c(obs, rates)))) {
            return('Please enter data')
        }
        if (!all(data.table::between(obs, 0, MAX_OBS))) {
            return('Please use less extreme number of observations')
        }
        if (!all(data.table::between(rates, 0, 1))) {
            return('Rates must be between 0 and 1')
        }
        return(data.table::data.table('variant' = c('a', 'b')
                                      , 'num_obs' = obs
                                      , 'observed_rate' = rates))
    } else if (dist_name == 'normal') {
        counts <- c(count_a, count_b)
        means <- c(mean_a, mean_b)
        sds <- c(sd_a, sd_b)
        if (any(is.na(c(counts, means, sds)))) {
            return('Please enter data')
        }
        if (!all(data.table::between(counts, 0, MAX_OBS))) {
            return('Please use less extreme number of observations')
        }
        if (!all(data.table::between(means, -MAX_VAL, MAX_VAL))) {
            return('Average must be less extreme')
        }
        if (!all(data.table::between(sds, 0, MAX_VAL, incbounds=FALSE))) {
            return('Standard deviations must be less extreme')
        }
        return(data.table::data.table('variant' = c('a', 'b')
                                      , 'num_obs' = counts
                                      , 'avg' = means
                                      , 'std_dev' = sds))
    } else {
        nums <- c(num_a, num_b)
        totals <- c(total_a, total_b)
        if (any(is.na(c(nums, totals)))) {
            return('Please enter data')
        }
        if (!all(data.table::between(nums, 0, MAX_VAL))) {
            return('Please use less extreme number of sessions')
        }
        if (!all(data.table::between(totals, 0, MAX_VAL))) {
            return('Please use less extreme number of observations')
        }
        return(data.table::data.table('variant' = c('a', 'b')
                                      , 'num_sessions' = c(num_a, num_b)
                                      , 'observed_count' = c(total_a, total_b)))
    }
}

create_description <- function(dist_name, dist) {
    if (is.character(dist)) {
        return('')
    } else {
        if (dist_name == 'binomial') {
            moments <- abayes::compute_moments(dist)
            mu <- moments[['mu']]; sigma <- moments[['sigma']]
            return(paste0('The distribution is a beta distribution with parameters: alpha = '
                         , round(dist[['alpha']], 2), ', beta = ', round(dist[['beta']], 2)
                         , '. Mean: ',  signif(mu, 3), ' and Standard Deviation: ', signif(sigma, 3)))
        } else if (dist_name == 'normal') {
            moments <- abayes::compute_moments(dist)
            x_mu <- moments[['x']][['mu']]; x_sigma <- moments[['x']][['sigma']]
            tau_mu <- moments[['tau']][['mu']]; tau_sigma <- moments[['tau']][['sigma']]
            return(paste0('The distribution is a normal gamma distribution with parameters: mu = '
                         , round(dist[['mu']], 2), ', lambda = ', round(dist[['lambda']], 2)
                         , ', alpha = ', round(dist[['alpha']], 2), ', beta = ', round(dist[['beta']], 2)
                         , '. Mean: ', signif(x_mu, 3), ' and Standard Deviation: ', signif(x_sigma, 3)))
        } else {
            moments <- abayes::compute_moments(dist)
            mu <- moments[['mu']]; sigma <- moments[['sigma']]
            return(paste0('The distribution is a gamma distribution with parameters: alpha = '
                          , round(dist[['alpha']], 2), ', beta = ', round(dist[['beta']], 2)
                          , '. Mean: ', signif(mu, 3), ' and Standard Deviation: ', signif(sigma, 3)))
        }
    }
}

create_metric_dt <- function(posteriors, method) {
    
    metrics <- abayes::get_metrics(posteriors = posteriors, sim_batch_size = 1e5, method = method)
    dt <- data.table::data.table(x = c('A', 'B')
                                 , y = as.character(signif(c(metrics[['loss_a']], metrics[['loss_b']]), 3))
                                 , z = as.character(signif(c(1 - metrics[['prob_b_gt_a']], metrics[['prob_b_gt_a']]), 3))
                                 , w = c('-', as.character(signif(metrics[['effect_lower']], 3)))
                                 , a = c('-', as.character(signif(metrics[['effect_expected']], 3)))
                                 , b = c('-', as.character(signif(metrics[['effect_upper']], 3)))
    )
    data.table::setnames(dt, c('Variant', 'Risk Of Choosing Variant', 'Prob Variant is Larger'
                               , '95% CI Lower Bound', 'Expected Effect Size (B - A)', '95% CI Upper Bound'))
    return(dt)
}

plot_dists <- function(dists, dist_name) {
    if (is.character(dists)) {
        df <- data.frame()
        return(ggplot(df) + geom_point() + xlim(0, 1) + ylim(0, 1) +
                   annotate('text', x = 0.5, y = 0.5, label = dists, size = 5))
    } else {
        if (dist_name == 'binomial') {
            return(plot_beta(betas = dists, title = 'What We Believe About the Rate'))
        } else if (dist_name == 'normal') {
            return(plot_normal(normals = dists))
        } else {
            return(plot_gamma(gammas = dists, title = 'What We Believe About the Expected Count'))
        }
    }
}

