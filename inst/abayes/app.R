library(shiny)
library(data.table)
library(purrr)
library(ggplot2)
library(gridExtra)
library(markdown)
library(abayes)

source('server_funs.R')
source('ui_elements.R')

ui <- navbarPage('Bayesian A/B Testing'
    , tabPanel('Basic',
        sidebarLayout(
            sidebarPanel(
                
                h3('Choose Your Metric', align = 'center', style="color:#f65335")
                , radioButtons(inputId = 'dist_name_basic', label = 'Which option describes your metric?'
                               , choices = dist_name_choices)
                
                , h3('Enter What You Believe Before The Experiment', align = 'center', style="color:#f65335")
                
                , conditionalPanel('input.dist_name_basic == "binomial"'
                    , choose_binomial_rate(variant = NULL, id_type = 'basic')
                    , choose_binomial_bound(variant = NULL, id_type = 'basic')
                    , br()
                    , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                    , h4('Variant A', align = 'center')
                    , set_binomial_obs(variant = 'a', id_type = 'basic')
                    , set_binomial_rate(variant = 'a', id_type = 'basic')
                    , h4('Variant B', align = 'center')
                    , set_binomial_obs(variant = 'b', id_type = 'basic')
                    , set_binomial_rate(variant = 'b', id_type = 'basic')
                    , br()
                    , h3('Choose Your Loss Function', align = 'center', style="color:#f65335")
                    , radioButtons(inputId = 'loss_type_basic', label = 'Choose between the absolute loss or the percent loss'
                                   , choices = list('absolute' = 'absolute', 'percent' = 'percent'))
                )
                
                , conditionalPanel('input.dist_name_basic == "normal"'
                    , choose_normal_mean(variant = NULL, id_type = 'basic')
                    , choose_normal_mean_bound(variant = NULL, id_type = 'basic')
                    , choose_normal_sd(variant = NULL, id_type = 'basic')
                    , choose_normal_sd_bound(variant = NULL, id_type = 'basic')
                    , br()
                    , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                    , h4('Variant A', align = 'center')
                    , set_normal_count(variant = 'a', id_type = 'basic')
                    , set_normal_mean(variant = 'a', id_type = 'basic')
                    , set_normal_sd(variant = 'a', id_type = 'basic')
                    , h4('Variant B', align = 'center')
                    , set_normal_count(variant = 'b', id_type = 'basic')
                    , set_normal_mean(variant = 'b', id_type = 'basic')
                    , set_normal_sd(variant = 'b', id_type = 'basic')
                )
                
                , conditionalPanel('input.dist_name_basic == "poisson"'
                   , choose_poisson_lambda(variant = NULL, id_type = 'basic')
                   , choose_poisson_max(variant = NULL, id_type = 'basic')
                   , br()
                   , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                   , h4('Variant A', align = 'center')
                   , set_poisson_num(variant = 'a', id_type = 'basic')
                   , set_poisson_total(variant = 'a', id_type = 'basic')
                   , h4('Variant B', align = 'center')
                   , set_poisson_num(variant = 'b', id_type = 'basic')
                   , set_poisson_total(variant = 'b', id_type = 'basic')
                )
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(
                
                h1('Before The Experiment', align = 'center', style="color:#f65335")
                , plotOutput(outputId = "plot_prior_basic")
                , textOutput(outputId = 'prior_description_basic')
                , h1('After Observing Some Data', align = 'center', style="color:#f65335")
                , conditionalPanel('(input.dist_name_basic == "binomial" && (input.obs_a_basic != 0 || input.obs_b_basic != 0))
                                    || (input.dist_name_basic == "normal" && (input.count_a_basic != 0 || input.count_b_basic != 0))
                                    || (input.dist_name_basic == "poisson" && (input.num_a_basic != 0 || input.num_b_basic != 0))'
                    , tableOutput(outputId = 'metrics_basic')
                    , plotOutput(outputId = "plot_posterior_basic")
                )
                , conditionalPanel('(input.dist_name_basic == "binomial" && input.obs_a_basic == 0 && input.obs_b_basic == 0)
                                    || (input.dist_name_basic == "normal" && input.count_a_basic == 0 && input.count_b_basic == 0)
                                    || (input.dist_name_basic == "poisson" && input.num_a_basic == 0 && input.num_b_basic == 0)'
                                   , br()
                                   , br()
                                   , h4(paste('In order to determine which variant is better, you need to enter'
                                              , 'the results of your experiment. Use the panel on the left.')
                                        , style="color:black")
                )
            )
        )
    )
    , tabPanel('Advanced',
        sidebarLayout(
            sidebarPanel(
                       
                h3('Choose Your Metric', align = 'center', style="color:#f65335")
                , radioButtons(inputId = 'dist_name_adv', label = 'Which option describes your metric?'
                               , choices = dist_name_choices)
                   
                , h3('Enter What You Believe Before The Experiment', align = 'center', style="color:#f65335")
                   
                , conditionalPanel('input.dist_name_adv == "binomial"'
                       , h4('Variant A', align = 'center')
                       , choose_binomial_rate(variant = 'a', id_type = 'adv')
                       , choose_binomial_bound(variant = 'a', id_type = 'adv')
                       , h4('Variant B', align = 'center')
                       , choose_binomial_rate(variant = 'b', id_type = 'adv')
                       , choose_binomial_bound(variant = 'b', id_type = 'adv')
                       , br()
                       , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                       , h4('Variant A', align = 'center')
                       , set_binomial_obs(variant = 'a', id_type = 'adv')
                       , set_binomial_rate(variant = 'a', id_type = 'adv')
                       , h4('Variant B', align = 'center')
                       , set_binomial_obs(variant = 'b', id_type = 'adv')
                       , set_binomial_rate(variant = 'b', id_type = 'adv')
                       , br()
                       , h3('Choose Your Loss Function', align = 'center', style="color:#f65335")
                       , radioButtons(inputId = 'loss_type_adv', label = 'Choose between the absolute loss or the percent loss'
                                      , choices = list('absolute' = 'absolute', 'percent' = 'percent'))
                   )
                   
                , conditionalPanel('input.dist_name_adv == "normal"'
                       , h4('Variant A', align = 'center')
                       , choose_normal_mean(variant = 'a', id_type = 'adv')
                       , choose_normal_mean_bound(variant = 'a', id_type = 'adv')
                       , choose_normal_sd(variant = 'a', id_type = 'adv')
                       , choose_normal_sd_bound(variant = 'a', id_type = 'adv')
                       , h4('Variant B', align = 'center')
                       , choose_normal_mean(variant = 'b', id_type = 'adv')
                       , choose_normal_mean_bound(variant = 'b', id_type = 'adv')
                       , choose_normal_sd(variant = 'b', id_type = 'adv')
                       , choose_normal_sd_bound(variant = 'b', id_type = 'adv')
                       , br()
                       , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                       , h4('Variant A', align = 'center')
                       , set_normal_count(variant = 'a', id_type = 'adv')
                       , set_normal_mean(variant = 'a', id_type = 'adv')
                       , set_normal_sd(variant = 'a', id_type = 'adv')
                       , h4('Variant B', align = 'center')
                       , set_normal_count(variant = 'b', id_type = 'adv')
                       , set_normal_mean(variant = 'b', id_type = 'adv')
                       , set_normal_sd(variant = 'b', id_type = 'adv')
                   )
                   
                , conditionalPanel('input.dist_name_adv == "poisson"'
                       , h4('Variant A', align = 'center')
                       , choose_poisson_lambda(variant = 'a', id_type = 'adv')
                       , choose_poisson_max(variant = 'a', id_type = 'adv')
                       , h4('Variant B', align = 'center')
                       , choose_poisson_lambda(variant = 'b', id_type = 'adv')
                       , choose_poisson_max(variant = 'b', id_type = 'adv')
                       , br()
                       , h3('Enter The Results of Your Experiment', align = 'center', style="color:#f65335")
                       , h4('Variant A', align = 'center')
                       , set_poisson_num(variant = 'a', id_type = 'adv')
                       , set_poisson_total(variant = 'a', id_type = 'adv')
                       , h4('Variant B', align = 'center')
                       , set_poisson_num(variant = 'b', id_type = 'adv')
                       , set_poisson_total(variant = 'b', id_type = 'adv')
                )
            ),
               
               # Main panel for displaying outputs ----
           mainPanel(
               
               h1('Before The Experiment', align = 'center', style="color:#f65335")
               , plotOutput(outputId = "plot_prior_adv")
               , textOutput(outputId = 'prior_description_adv')
               , h1('After Observing Some Data', align = 'center', style="color:#f65335")
               , conditionalPanel('(input.dist_name_adv == "binomial" && (input.obs_a_adv != 0 || input.obs_b_adv != 0))
                                  || (input.dist_name_adv == "normal" && (input.count_a_adv != 0 || input.count_b_adv != 0))
                                  || (input.dist_name_adv == "poisson" && (input.num_a_adv != 0 || input.num_b_adv != 0))'
                                  , tableOutput(outputId = 'metrics_adv')
                                  , plotOutput(outputId = "plot_posterior_adv")
                                  , textOutput(outputId = 'posterior_description_adv')
                                  # , plotOutput(outputId = 'relative_gain_adv')
               )
               , conditionalPanel('(input.dist_name_adv == "binomial" && input.obs_a_adv == 0 && input.obs_b_adv == 0)
                                  || (input.dist_name_adv == "normal" && input.count_a_adv == 0 && input.count_b_adv == 0)
                                  || (input.dist_name_adv == "poisson" && input.num_a_adv == 0 && input.num_b_adv == 0)'
                                  , br()
                                  , br()
                                  , h4(paste('In order to determine which variant is better, you need to enter'
                                             , 'the results of your experiment. Use the panel on the left.')
                                       , style="color:black")
               )
           )
        )
    )
    , tabPanel('How To Use',
        fluidRow(column(12, includeMarkdown('how_to.md')))
    )
)

# generate the outputs
server <- function(input, output) {
    
    prior_dist_basic <- reactive({
        prior_dist <- create_prior_dist(dist_name = input$dist_name_basic
                                        , expected_rate = input$expected_rate_basic
                                        , upper_bound_rate = input$upper_bound_basic
                                        , expected_mean = input$expected_mean_basic
                                        , upper_bound_mean = input$upper_bound_mean_basic
                                        , expected_sd = input$expected_sd_basic
                                        , upper_bound_sd = input$upper_bound_sd_basic
                                        , expected_lambda = input$expected_lambda_basic
                                        , upper_bound_lambda = input$max_lambda_basic
        )
        prior_dist
    })
    
    prior_dist_adv <- reactive({
        prior_dist_a <- create_prior_dist(dist_name = input$dist_name_adv
                                          , expected_rate = input$expected_rate_a_adv
                                          , upper_bound_rate = input$upper_bound_a_adv
                                          , expected_mean = input$expected_mean_a_adv
                                          , upper_bound_mean = input$upper_bound_mean_a_adv
                                          , expected_sd = input$expected_sd_a_adv
                                          , upper_bound_sd = input$upper_bound_sd_a_adv
                                          , expected_lambda = input$expected_lambda_a_adv
                                          , upper_bound_lambda = input$max_lambda_a_adv
        )
        prior_dist_b <- create_prior_dist(dist_name = input$dist_name_adv
                                          , expected_rate = input$expected_rate_b_adv
                                          , upper_bound_rate = input$upper_bound_b_adv
                                          , expected_mean = input$expected_mean_b_adv
                                          , upper_bound_mean = input$upper_bound_mean_b_adv
                                          , expected_sd = input$expected_sd_b_adv
                                          , upper_bound_sd = input$upper_bound_sd_b_adv
                                          , expected_lambda = input$expected_lambda_b_adv
                                          , upper_bound_lambda = input$max_lambda_b_adv
        )
        if (is.character(prior_dist_a)) {
            paste('Variant A:', prior_dist_a)
        } else if (is.character(prior_dist_b)) {
            paste('Variant B:', prior_dist_b)
        } else {
            list('a' = prior_dist_a, 'b' = prior_dist_b)
        }
    })
    
    results_dt_basic <- reactive({
        results_dt <- create_results_dt(dist_name = input$dist_name_basic
                                        , obs_a = input$obs_a_basic, obs_b = input$obs_b_basic
                                        , rate_a = input$rate_a_basic, rate_b = input$rate_b_basic
                                        , count_a = input$count_a_basic, count_b = input$count_b_basic
                                        , mean_a = input$mean_a_basic, mean_b = input$mean_b_basic
                                        , sd_a = input$sd_a_basic, sd_b = input$sd_b_basic
                                        , num_a = input$num_a_basic, num_b = input$num_b_basic
                                        , total_a = input$total_a_basic, total_b = input$total_b_basic)
        results_dt
    })
    
    results_dt_adv <- reactive({
        results_dt <- create_results_dt(dist_name = input$dist_name_adv
                                        , obs_a = input$obs_a_adv, obs_b = input$obs_b_adv
                                        , rate_a = input$rate_a_adv, rate_b = input$rate_b_adv
                                        , count_a = input$count_a_adv, count_b = input$count_b_adv
                                        , mean_a = input$mean_a_adv, mean_b = input$mean_b_adv
                                        , sd_a = input$sd_a_adv, sd_b = input$sd_b_adv
                                        , num_a = input$num_a_adv, num_b = input$num_b_adv
                                        , total_a = input$total_a_adv, total_b = input$total_b_adv)
        results_dt
    })
    
    posterior_basic <- reactive({
        prior_dist <- prior_dist_basic()
        results_dt <- results_dt_basic()
        
        if (is.character(prior_dist)) {
            return(prior_dist)
        }
            
        if (is.character(results_dt)) {
            return(results_dt)
        }
        priors <- list('a' = prior_dist, 'b' = prior_dist)
        update_priors(priors = priors, stats_dt = results_dt)
    })
    
    posterior_adv <- reactive({
        prior_dist <- prior_dist_adv()
        results_dt <- results_dt_adv()
        
        if (is.character(prior_dist)) {
            return(prior_dist)
        }
        
        if (is.character(results_dt)) {
            return(results_dt)
        }
        update_priors(priors = prior_dist, stats_dt = results_dt)
    })
    
    metrics_dt_basic <- reactive({
        posteriors <- posterior_basic()
        if (is.character(posteriors)) {
            return(data.table::data.table())
        }
        
        create_metric_dt(posteriors = posteriors, method = input$loss_type_basic)
    })
    
    metrics_dt_adv <- reactive({
        posteriors <- posterior_adv()
        if (is.character(posteriors)) {
            return(data.table::data.table())
        }
        create_metric_dt(posteriors = posteriors, method = input$loss_type_adv)
    })
    
    # prior plot
    output$plot_prior_basic <- renderPlot({
        prior_dist <- prior_dist_basic()
        if (!is.character(prior_dist)) {
            prior_dist <- list('a' = prior_dist)
        }
        plot_dists(dists = prior_dist, input$dist_name_basic)
    })
    
    output$plot_prior_adv <- renderPlot({
        plot_dists(dists = prior_dist_adv(), input$dist_name_adv)
    })
    
    # prior description
    output$prior_description_basic <- renderText({
        create_description(dist_name = input$dist_name_basic, dist = prior_dist_basic())
    })
    
    output$prior_description_adv <- renderText({
        prior_dist <- prior_dist_adv()
        if (is.character(prior_dist)) {
            return('')
        }
        
        a <- create_description(dist_name = input$dist_name_adv, dist = prior_dist[['a']])
        b <- create_description(dist_name = input$dist_name_adv, dist = prior_dist[['b']])
        return(paste0('Variant A: ', a, '. Variant B: ', b))
    })
    
    # posterior plot
    output$plot_posterior_basic <- renderPlot({
        plot_dists(dists = posterior_basic(), input$dist_name_basic)
    })
    
    output$plot_posterior_adv <- renderPlot({
        plot_dists(dists = posterior_adv(), input$dist_name_adv)
    })
    
    output$metrics_basic <- renderTable({
        metrics_dt_basic()
    }, digits = 7, striped = TRUE, hover = FALSE, bordered = FALSE
     , spacing = 'm', width = '90%', size = '40', align = 'c')
    
    output$metrics_adv <- renderTable({
        metrics_dt_adv()
    }, digits = 7, striped = TRUE, hover = FALSE, bordered = FALSE
    , spacing = 'm', width = '90%', size = '40', align = 'c')
    
    output$posterior_description_adv <- renderText({
        posterior <- posterior_adv()
        if (is.character(posterior)) {
            return('')
        }
        
        a <- create_description(dist_name = input$dist_name_adv, dist = posterior[['a']])
        b <- create_description(dist_name = input$dist_name_adv, dist = posterior[['b']])
        return(paste0('Variant A: ', a, '. Variant B: ', b))
    })
    
    # output$relative_gain_adv <- renderPlot({
    #     dists <- posterior_adv()
    #     plot_relative_gain(dists = dists)
    # })
    
}

shinyApp(ui = ui, server = server)




