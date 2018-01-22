#' @title Decide whether to stop the experiment using the last loss values
#' @name single_loss_stop
#' @description This function decides whether or not to stop the experiment and
#'              declare one of the variants as the winner.
#' @param losses A named vector of the losses for each variant.
#' @param loss_threshold Choose a variant once the loss goes beneath this value.
#' @export
#' @return A list containg two named elements: \code{'stop'} (a boolean) and
#'         \code{'winner'}, which is \code{NULL} if \code{'stop'} is \code{FALSE}
#'         else it is the name of the winning variant
single_loss_stop <- function(losses, loss_threshold) {
    if (any(losses < loss_threshold)) {
        best_variant <- names(losses)[which.min(losses)]
        return(list(stop = TRUE, variant = best_variant))
    } else {
        return(list(stop = FALSE, variant = NULL))
    }
}