#' @title Bayesian A/B Arguments
#' @name ab_arguments
#' @description Provide documentation for arguments commonly used in this package
#' @param data_dists A named list of distribution objects. This list specifies the
#'                   distributions that are used to generate data in simulations.
#'                   Currently, the list must only have elements named \code{'a'}
#'                   and \code{'b'}. See Details for more information.
#' @param priors A named list of distribution objects. This list specifies the
#'               distributions that are used as priors when estimating some parameter
#'               from the data generating distribution. Currently, the list must only
#'               have elements named \code{'a'} and \code{'b'}. See Details for more information.
#' @param loss_threshold A positive number that identifies a bound for the expected
#'                       loss for each variant. Once the expected loss is below this
#'                       bound, the experiment is concluded.
#' @param obs_per_round A positive number that represents how many observations, across both variants,
#'                      are generated before we update the prior distributions and
#'                      evaluate the expected loss. This number must be divisible by
#'                      the number of variants used. Default is \code{1000}.
#' @param max_rounds A positive integer that specifies the maximum number of
#'                   times that we will evaluate the expected loss on both experiments.
#'                   Default is \code{100}.
#' @param sim_batch_size A positive integer that specifies how much data is simulated
#'                       when evaluating the expected loss for variants that do
#'                       not have an analytic solution (i.e. normal data).
NULL


#' @title Create an Empty Data Table
#' @name create_empty_dt
#' @description Create an empty data table with a specified number of rows and columns.
#'              You can also choose the value that fills the data table.
#' @export
#' @importFrom data.table as.data.table setnames
#' @param num_rows A positive integer that specifies the number of rows for the data table
#' @param column_names A character vector that specifies the number and names of the columns.
#' @param fill_value Default is -1. Specifies what value should fill the data table
#' @return A data.table
create_empty_dt <- function(num_rows, column_names, fill_value = -1) {
    dt <- data.table::as.data.table(matrix(fill_value, nrow = num_rows, ncol = length(column_names)))
    data.table::setnames(dt, column_names)
    return(dt)
}
