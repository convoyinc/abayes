#' @title Plot Beta Distributions
#' @name plot_beta
#' @description This function plots the densities of multiple beta distributions
#' @export
#' @importFrom data.table between data.table rbindlist
#' @importFrom stats rbeta
#' @importFrom ggplot2 ggplot geom_density ggtitle xlab ylab scale_color_manual scale_fill_manual theme
#' @param betas A list of lists of beta distributions
#' @param title The title of the plot
#' @param xlab The title of the x axis
#' @param ylab The title of the y axis
#' @param color The color for the plot
#' @param level The desired amount of area between the lower and upper bounds. Default is \code{0.99}.
#' 
#' @return NULL. A plot is generated
plot_beta <- function(betas
                      , title = 'Beta Distribution'
                      , xlab = 'Rate that the Event Occurs'
                      , ylab = 'Density of that Rate'
                      , color = '#f65335'
                      , level = 0.99
) {
    
    n_samp <- 1e5
    beta_dt <- NULL
    
    for (i in seq_along(betas)) {
        x <- names(betas)[i]
        var_name <- paste('variant', x)
        
        # generate some data
        beta_vec <- stats::rbeta(n_samp, betas[[x]][['alpha']], betas[[x]][['beta']])
        
        # remove the lower and upper extremes of the data
        beta_vec <- beta_vec[data.table::between(beta_vec
                                                 , quantile(beta_vec, 0.005)
                                                 , quantile(beta_vec, 0.995))]
        
        beta_dt <- data.table::rbindlist(list(beta_dt, data.table::data.table('variant' = rep(var_name, length(beta_vec))
                                                                              , 'betas' = beta_vec)))
    }
    
    if (length(betas) > 1) {
        col_vals <- c('darkred', 'darkblue')
    } else {
        col_vals <- color
    }
    
    rate_plot <- ggplot(beta_dt, aes(x = betas, colour = variant, fill =variant)) + 
        geom_density(size = 1, alpha = 0.1) +
        ggtitle(title) + xlab(xlab) + ylab(ylab) +
        scale_color_manual(values = col_vals) +
        scale_fill_manual(values = col_vals) +
        theme(plot.title = element_text(hjust = 0.5, size = 22)
              , axis.title = element_text(size = 18)
              , axis.text = element_text(size = 14)
              , legend.title = element_blank()
              , legend.text = element_text(size = 16)
              , legend.position = if(length(betas) == 2) 'bottom' else 'none')
    return(rate_plot)
}

#' @title Plot Normal Distributions
#' @name plot_normal
#' @description This function allows you to visualize the densities of a normal distribution
#' @export
#' @importFrom stats rgamma rnorm
#' @importFrom data.table between data.table rbindlist
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @param normals A list of lists of normal distributions
#' @inheritParams plot_beta
#' @return NULL. A plot is generated
plot_normal <- function(normals) {
    n_samp <- 1e5
    
    sd_dt <- NULL
    mu_dt <- NULL
    
    # sample some data
    for (i in seq_along(normals)) {
        x <- names(normals)[i]
        var_name <- paste('variant', x)
        
        # generate some data
        sd_vec <- sqrt(1 / stats::rgamma(n_samp
                                         , normals[[x]][['alpha']]
                                         , normals[[x]][['beta']]))
        mu_vec <- stats::rnorm(n_samp
                               , normals[[x]][['mu']]
                               , sqrt(1 / normals[[x]][['lambda']]) * sd_vec)
        
        # remove the lower and upper extremes of the data
        sd_vec <- sd_vec[data.table::between(sd_vec
                                             , quantile(sd_vec, 0.005)
                                             , quantile(sd_vec, 0.995))]
        n_sd <- length(sd_vec)
        mu_vec <- mu_vec[data.table::between(mu_vec
                                             , quantile(mu_vec, 0.005)
                                             , quantile(mu_vec, 0.995))]
        n_mu <- length(mu_vec)
        
        sd_dt <- data.table::rbindlist(list(sd_dt, data.table::data.table('variant' = rep(var_name, n_sd)
                                                                          , 'sds' = sd_vec)))
        mu_dt <- data.table::rbindlist(list(mu_dt, data.table::data.table('variant' = rep(var_name, n_mu)
                                                                          , 'mus' = mu_vec)))
    }
    
    if (length(normals) > 1) {
        col_vals <- c('darkred', 'darkblue')
    } else {
        col_vals <- 'black'
    }
    
    mu_plot <- ggplot(mu_dt, aes(x = mus, colour = variant, fill = variant)) + 
        geom_density(size = 1, alpha = 0.1) +
        ggtitle("What We Believe About The Mean") +
        xlab('Mean') +
        ylab('Probability') +
        scale_color_manual(values = col_vals) +
        scale_fill_manual(values = col_vals) +
        theme(plot.title = element_text(hjust = 0.5, size = 22)
              , axis.title = element_text(size = 18)
              , axis.text = element_text(size = 14)
              , legend.title = element_blank()
              , legend.text = element_text(size = 16)
              , legend.position = 'none')
    
    sd_plot <- ggplot(sd_dt, aes(x = sds, colour = variant, fill = variant)) + 
        geom_density(size = 1, alpha = 0.1) +
        ggtitle("What We Believe About The Standard Deviation") +
        xlab('Standard Deviation') +
        ylab('Probability') +
        scale_color_manual(values = col_vals) +
        scale_fill_manual(values = col_vals) +
        theme(plot.title = element_text(hjust = 0.5, size = 22)
              , axis.title = element_text(size = 18)
              , axis.text = element_text(size = 14)
              , legend.title = element_blank()
              , legend.text = element_text(size = 16)
              , legend.position = if(length(normals) == 2) 'bottom' else 'none')
    
    if (length(normals) == 2) {
        g_legend <- function(a.gplot) {
            tmp <- ggplot_gtable(ggplot_build(a.gplot))
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            legend <- tmp$grobs[[leg]]
            return(legend)
        }
        
        my_legend <- g_legend(sd_plot)
        
        results_plot <- gridExtra::grid.arrange(gridExtra::arrangeGrob(mu_plot + theme(legend.position="none"),
                                                                       sd_plot + theme(legend.position="none"),
                                                                       nrow = 2)
                                                , my_legend, nrow = 2, heights=c(10, 1))
    } else {
        results_plot <- gridExtra::grid.arrange(mu_plot, sd_plot, ncol = 1, heights=c(1, 1))
    }
    
    return(NULL)
}

#' @title Plot Gamma Distributions
#' @name plot_gamma
#' @description This function plots the densities of multiple gamma distributions
#' @export
#' @importFrom purrr map map_dbl
#' @param gammas A list of lists of gamma distributions
#' @inheritParams plot_beta
#' @return NULL. A plot is generated
plot_gamma <- function(gammas, title = 'Density of Gamma Distribution', level = 0.99) {
    
    n_samp <- 1e5
    gamma_dt <- NULL
    
    for (i in seq_along(gammas)) {
        x <- names(gammas)[i]
        var_name <- paste('variant', x)
        
        # generate some data
        gamma_vec <- rgamma(n_samp, gammas[[x]][['alpha']], gammas[[x]][['beta']])
        
        # remove the lower and upper extremes of the data
        gamma_vec <- gamma_vec[data.table::between(gamma_vec
                                                   , quantile(gamma_vec, 0.005)
                                                   , quantile(gamma_vec, 0.995))]
        
        gamma_dt <- data.table::rbindlist(list(gamma_dt, data.table::data.table('variant' = rep(var_name, length(gamma_vec))
                                                                                , 'gammas' = gamma_vec)))
    }
    
    if (length(gammas) > 1) {
        col_vals <- c('darkred', 'darkblue')
    } else {
        col_vals <- 'black'
    }
    
    lambda_plot <- ggplot(gamma_dt, aes(x = gammas, colour = variant, fill =variant)) + 
        geom_density(size = 1, alpha = 0.1) +
        ggtitle(title) +
        xlab('Expected Amount of Times Event Occurrs') +
        ylab('Probability of That Rate') +
        scale_color_manual(values = col_vals) +
        scale_fill_manual(values = col_vals) +
        theme(plot.title = element_text(hjust = 0.5, size = 22)
              , axis.title = element_text(size = 18)
              , axis.text = element_text(size = 14)
              , legend.title = element_blank()
              , legend.text = element_text(size = 16)
              , legend.position = if(length(gammas) == 2) 'bottom' else 'none')
    return(lambda_plot)
}

