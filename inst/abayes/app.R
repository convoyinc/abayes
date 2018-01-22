library(shiny)
library(data.table)
library(purrr)
library(ggplot2)
library(gridExtra)
library(markdown)
library(abayes)

ui <- navbarPage('Bayesian A/B Testing'
    , tabPanel('Main',
        sidebarLayout(
            sidebarPanel(
                
                h3('Choose Your Metric', align = 'center', style="color:#f65335")
                
                # allow the user to choose the name of the experiment variable
                , radioButtons(inputId = 'dist_name', label = 'Which option describes your metric?'
                               , choices = list('Rate: Your metric is whether an event happens or not.' = 'binomial'
                                                , 'Quantity: Your metric is a normally distributed, continuous quantity.' = 'normal'
                                                , 'Count: Your metric is the number of times an event happens.' = 'poisson'))
                
                , h3('Enter What You Believe Before The Experiment', align = 'center', style="color:#f65335")
                
                , conditionalPanel('input.dist_name == "binomial"'
                    
                    , numericInput(inputId = "expected_rate"
                                 , label = "Expected Rate (probability that the event happens):"
                                 , value = 0.1
                                 , min = 0
                                 , max = 1
                                 , step = 0.01)
                    
                    , numericInput(inputId = "upper_bound"
                                   , label = "Maximum Possible Rate (the max value ever observed):"
                                   , value = 0.2
                                   , min = 0
                                   , max = 1
                                   , step = 0.01)
                    
                    , br()
                    
                    , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                    
                    , h4('Variant A', align = 'center')
                    
                    , numericInput(inputId = "obs_a"
                                   , label = "Number of Observations:"
                                   , value = 0
                                   , min = 0
                                   , max = Inf
                                   , step = 1)
                    
                    , numericInput(inputId = "rate_a"
                                   , label = "Observed Rate (proportion of times the event happened):"
                                   , value = 0.1
                                   , min = 0
                                   , max = 1
                                   , step = 0.01)
                    
                    , h4('Variant B', align = 'center')
                    
                    , numericInput(inputId = "obs_b"
                                   , label = "Number of Observations:"
                                   , value = 0
                                   , min = 0
                                   , max = Inf
                                   , step = 1)
                    
                    , numericInput(inputId = "rate_b"
                                   , label = "Observed Rate (proportion of times the event happened):"
                                   , value = 0.1
                                   , min = 0
                                   , max = 1
                                   , step = 0.01)
                )
                
                , conditionalPanel('input.dist_name == "normal"'
                                   
                    , numericInput(inputId = "expected_mean"
                                  , label = "Expected Average:"
                                  , value = 0
                                  , min = -Inf
                                  , max = Inf
                                  , step = 1)
                    
                    , numericInput(inputId = "upper_bound_mean"
                                  , label = "Maximum Possible Average:"
                                  , value = 1
                                  , min = -Inf
                                  , max = Inf
                                  , step = 1)
                    
                    , numericInput(inputId = "expected_sd"
                                  , label = "Expected Standard Deviation:"
                                  , value = 1
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                    
                    , numericInput(inputId = "upper_bound_sd"
                                  , label = "Maximum Possible Standard Deviation:"
                                  , value = 2
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                    
                    , br()
                    
                    , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                    
                    , h4('Variant A', align = 'center')
                    
                    , numericInput(inputId = "count_a"
                                  , label = "Number of Observations:"
                                  , value = 0
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                    
                    , numericInput(inputId = "mean_a"
                                  , label = "Observed Mean:"
                                  , value = 0
                                  , min = -Inf
                                  , max = Inf
                                  , step = 1)
                    
                    , numericInput(inputId = "sd_a"
                                  , label = "Observed Standard Deviation:"
                                  , value = 1
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                    
                    , h4('Variant B', align = 'center')
                    
                    , numericInput(inputId = "count_b"
                                  , label = "Number of Observations:"
                                  , value = 0
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                    
                    , numericInput(inputId = "mean_b"
                                  , label = "Observed Mean:"
                                  , value = 0
                                  , min = -Inf
                                  , max = Inf
                                  , step = 1)
                    
                    , numericInput(inputId = "sd_b"
                                  , label = "Observed Standard Deviation:"
                                  , value = 1
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                )
                
                , conditionalPanel('input.dist_name == "poisson"'
                                   
                   , numericInput(inputId = "expected_lambda"
                                  , label = "Expected Count (number of events in a session):"
                                  , value = 1
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                   
                   , numericInput(inputId = "max_lambda"
                                  , label = "Maximum Possible Count (the max count ever observed):"
                                  , value = 3
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                   
                   , br()
                   
                   , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                   
                   , h4('Variant A', align = 'center')
                   
                   , numericInput(inputId = "num_a"
                                  , label = "Number of Sessions:"
                                  , value = 0
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                   
                   , numericInput(inputId = "total_a"
                                  , label = "Observed Number of Events (counted over all sessions):"
                                  , value = 0
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                   
                   , h4('Variant B', align = 'center')
                   
                   , numericInput(inputId = "num_b"
                                  , label = "Number of Sessions:"
                                  , value = 0
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                   
                   , numericInput(inputId = "total_b"
                                  , label = "Observed Number of Events (counted over all sessions):"
                                  , value = 0
                                  , min = 0
                                  , max = Inf
                                  , step = 1)
                )
                
                , br()
                
                , h3('Control Your Metrics', align = 'center')
                
                # allow the user to select from different loss functions
                , radioButtons(inputId = 'loss_type', label = 'Choose Your Loss Function'
                               , choices = list('absolute' = 'absolute', 'percent' = 'percent'))
                
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(
                
                h1('Before The Experiment', align = 'center', style="color:#f65335")
                , plotOutput(outputId = "plot_prior")
                , textOutput(outputId = 'prior_description')
                , h1('After Observing Some Data', align = 'center', style="color:#f65335")
                , conditionalPanel('(input.dist_name == "binomial" && (input.obs_a != 0 || input.obs_b != 0))
                                    || (input.dist_name == "normal" && (input.count_a != 0 || input.count_b != 0))
                                    || (input.dist_name == "poisson" && (input.num_a != 0 || input.num_b != 0))'
                    , tableOutput(outputId = 'metrics')
                    , plotOutput(outputId = "plot_posterior")
                )
                , conditionalPanel('(input.dist_name == "binomial" && input.obs_a == 0 && input.obs_b == 0)
                                    || (input.dist_name == "normal" && input.count_a == 0 && input.count_b == 0)
                                    || (input.dist_name == "poisson" && input.num_a == 0 && input.num_b == 0)'
                                   , br()
                                   , br()
                                   , h4(paste('In order to determine which variant is better, you need to enter'
                                              , 'the results of your experiment. Use the panel on the left.')
                                        , style="color:black")
                )
                # , plotOutput(outputId = "plot_relative")
                
            )
        )
    )
    , tabPanel('How To Use',
        fluidRow(
            column(12
                , includeMarkdown('how_to.md')
            )
        )
    )
)

# generate the outputs
server <- function(input, output) {
    
    prior_dist <- reactive({
        if (input$dist_name == 'binomial') {
            expected <- input$expected_rate
            bound <- input$upper_bound
            if (is.na(expected) || is.na(bound)) {
                return('Please enter data')
            }
            prior_dist <- get_supported_beta(mu = expected, bound = bound, desired_support = 0.05)
        } else if (input$dist_name == 'normal') {
            expected_mean <- input$expected_mean
            bound_mean <- input$upper_bound_mean
            expected_sd <- input$expected_sd
            bound_sd <- input$upper_bound_sd
            if (is.na(expected_mean) || is.na(bound_mean) || is.na(expected_sd) || is.na(bound_sd)) {
                return('Please enter data')
            }
            prior_dist <- get_supported_normal_gamma(mu = expected_mean, bound_mu = bound_mean
                                                     , sigma = expected_sd, bound_sigma = bound_sd
                                                     , desired_support = 0.05)
        } else {
            expected <- input$expected_lambda
            bound <- input$max_lambda
            if (is.na(expected) || is.na(bound)) {
                return('Please enter data')
            }
            prior_dist <- get_supported_gamma(mu = expected, bound = bound, desired_support = 0.05)
        }
        prior_dist
    })
    
    results_dt <- reactive({
        if (input$dist_name == 'binomial') {
            results_dt <- data.table::data.table('variant' = c('a', 'b')
                                                 , 'num_obs' = c(input$obs_a, input$obs_b)
                                                 , 'observed_rate' = c(input$rate_a, input$rate_b))
        } else if (input$dist_name == 'normal') {
            results_dt <- data.table::data.table('variant' = c('a', 'b')
                                                 , 'num_obs' = c(input$count_a, input$count_b)
                                                 , 'avg' = c(input$mean_a, input$mean_b)
                                                 , 'std_dev' = c(input$sd_a, input$sd_b))
        } else {
            results_dt <- data.table::data.table('variant' = c('a', 'b')
                                                 , 'num_sessions' = c(input$num_a, input$num_b)
                                                 , 'observed_count' = c(input$total_a, input$total_b))
        }
        results_dt
    })
    
    posterior <- reactive({
        if (is.character(prior_dist())) {
            return(NULL)
        }
        priors <- list('a' = prior_dist(), 'b' = prior_dist())
        posteriors <- update_priors(priors = priors
                                    , stats_dt = results_dt())
        posteriors
    })
    
    metrics_dt <- reactive({
        if (is.null(posterior())) {
            return(NULL)
        }
        
        metrics <- get_metrics(posteriors = posterior(), sim_batch_size = 1e5, method = input$loss_type)
        if (is.null(metrics)) {
            return(NULL)
        }
        dt <- data.table::data.table(x = c('A', 'B')
                                     , y = as.character(signif(c(metrics[['loss_a']], metrics[['loss_b']]), 3))
                                     , z = as.character(signif(c(1 - metrics[['prob_b_gt_a']], metrics[['prob_b_gt_a']]), 3))
                                     , w = c('-', as.character(signif(metrics[['effect_lower']], 3)))
                                     , a = c('-', as.character(signif(metrics[['effect_expected']], 3)))
                                     , b = c('-', as.character(signif(metrics[['effect_upper']], 3)))
        )
        data.table::setnames(dt, c('Variant', 'Risk Of Choosing Variant', 'Prob Variant is Larger'
                                   , '95% CI Lower Bound', 'Expected Effect Size (B - A)', '95% CI Upper Bound'))
        dt
    })
    
    # prior plot
    output$plot_prior <- renderPlot({
        if (is.character(prior_dist())) {
            df <- data.frame()
            ggplot(df) + geom_point() + xlim(0, 1) + ylim(0, 1) +
                annotate('text', x = 0.5, y = 0.5, label = prior_dist(), size = 5)
        } else {
            if (input$dist_name == 'binomial') {
                plot_beta(list('a' = prior_dist()), title = 'What We Believe About The Rate')
            } else if (input$dist_name == 'normal') {
                plot_normal(list('a' = prior_dist()))
            } else {
                plot_gamma(list('a' = prior_dist()), title = 'What We Believe About the Expected Count')
            }
        }
    })
    
    # prior description
    output$prior_description <- renderText({
        if (is.character(prior_dist())) {
            ''
        } else {
            if (input$dist_name == 'binomial') {
                alpha <- prior_dist()[['alpha']]
                beta <- prior_dist()[['beta']]
                sd <- sqrt(alpha * beta / (alpha + beta) ^ 2 / (alpha + beta + 1))
                paste('We have chosen prior from the beta distribution with parameters: alpha = '
                      , round(alpha, 2), ', beta = ', round(beta, 2)
                      , '. This distribution has a standard deviation of', signif(sd, 3))
            } else if (input$dist_name == 'normal') {
                mu <- prior_dist()[['mu']]
                lambda <- prior_dist()[['lambda']]
                alpha <- prior_dist()[['alpha']]
                beta <- prior_dist()[['beta']]
                
                sigma_mu <- sqrt(alpha / beta)
                sigma_tau <- sqrt(alpha / beta ^ 2)
                paste('We have chosen prior from the normal gamma distribution with parameters: mu = '
                      , round(mu, 2), ', lambda = ', round(lambda, 2), ', alpha = ', round(alpha, 2)
                      , ', beta = ', round(beta, 2)
                      , '. This distribution has a standard deviation of', signif(sigma_mu, 3), 'for the'
                      , 'mean and a standard deviation of', signif(sigma_tau, 2), 'for the standard deviation')
            } else {
                alpha <- prior_dist()[['alpha']]
                beta <- prior_dist()[['beta']]
                sigma <- sqrt(alpha / beta ^ 2)
                paste('We have chosen a prior from the gamma distribution with parameters: alpha = '
                      , round(alpha, 2), ', beta = ', round(beta, 2), '. This distribution has a standard'
                      , 'deviation of', signif(sigma, 3))
            }
        }
    })
    
    # posterior plot
    output$plot_posterior <- renderPlot({
        if (is.null(posterior())) {
            df <- data.frame()
            ggplot(df) + geom_point() + xlim(0, 1) + ylim(0, 1)
        } else {
            if (input$dist_name == 'binomial') {
                plot_beta(betas = posterior(), title = 'What We Believe About the Rate for Each Variant')
            } else if (input$dist_name == 'normal') {
                plot_normal(normals = posterior())
            } else {
                plot_gamma(gammas = posterior(), title = 'What We Believe About the Expected Count For Each Variant')
            }
        }
    })
    
    output$metrics <- renderTable({
        metrics_dt()
    }, digits = 7, striped = TRUE, hover = FALSE, bordered = FALSE
    , spacing = 'm', width = '90%', size = '40', align = 'c')
    
    output$plot_relative <- renderPlot({
        plot_relative_gain(betas = posterior())
    })
    
}

shinyApp(ui = ui, server = server)




