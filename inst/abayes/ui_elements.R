dist_name_choices <- list('Rate: Your metric is the frequency that an event happens.' = 'binomial'
                          , 'Quantity: Your metric is a normally distributed, continuous quantity.' = 'normal'
                          , 'Count: Your metric is the number of times an event happens.' = 'poisson')

get_input_id <- function(id_string, variant, id_type) {
    if (is.null(variant)) {
        return(paste(id_string, id_type, sep = '_'))
    } else {
        return(paste(id_string, variant, id_type, sep = '_'))
    }
}

choose_binomial_rate <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("expected_rate", variant, id_type)
                        , label = "Expected Rate of the Event:"
                        , value = 0.1, min = 0, max = 1, step = 0.01))
}

choose_binomial_bound <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("upper_bound", variant, id_type)
                        , label = "Maximum Possible Rate:"
                        , value = 0.2, min = 0, max = 1, step = 0.01))
}

set_binomial_obs <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("obs", variant, id_type)
                        , label = "Number of Observations:"
                        , value = 0, min = 0, max = 1e8, step = 1))
}

set_binomial_rate <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("rate", variant, id_type)
                        , label = "Observed Frequency of the Event:"
                        , value = 0.1, min = 0, max = 1, step = 0.01))
}

choose_normal_mean <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id('expected_mean', variant, id_type)
                        , label = "Expected Average:"
                        , value = 0, min = -1e6, max = 1e6, step = 1))
}

choose_normal_mean_bound <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("upper_bound_mean", variant, id_type)
                        , label = "Maximum Possible Average:"
                        , value = 1, min = -1e6, max = 1e6, step = 1))
}

choose_normal_sd <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("expected_sd", variant, id_type)
                        , label = "Expected Standard Deviation:"
                        , value = 1, min = 0, max = 1e6, step = 1))
}

choose_normal_sd_bound <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("upper_bound_sd", variant, id_type)
                        , label = "Maximum Possible Standard Deviation:"
                        , value = 2, min = 0, max = 1e6, step = 1))
}

set_normal_count <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("count", variant, id_type)
                        , label = "Number of Observations:"
                        , value = 0, min = 0, max = 1e8, step = 1))
}

set_normal_mean <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("mean", variant, id_type)
                        , label = "Observed Average:"
                        , value = 0, min = -1e6, max = 1e6, step = 1))
}

set_normal_sd <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("sd", variant, id_type)
                        , label = "Observed Standard Deviation:"
                        , value = 1, min = 0, max = 1e6, step = 1))
}

choose_poisson_lambda <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("expected_lambda", variant, id_type)
                        , label = "Expected Number of Events in One Session:"
                        , value = 1, min = 0, max = 1e3, step = 1))
}

choose_poisson_max <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("max_lambda", variant, id_type)
                        , label = "Maximum Possible Events in One Session:"
                        , value = 3, min = 0, max = 1e3, step = 1))
}

set_poisson_num <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("num", variant, id_type)
                        , label = "Number of Sessions:"
                        , value = 0, min = 0, max = 1e5, step = 1))
}

set_poisson_total <- function(variant, id_type) {
    return(numericInput(inputId = get_input_id("total", variant, id_type)
                        , label = "Observed Number of Events Across All Sessions:"
                        , value = 0, min = 0, max = 1e8, step = 1))
}
