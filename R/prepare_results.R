##' Analysis results preparation
##'
##' This function prepares results to be used by \code{explor}. Not to be used directly.
##' 
##' @param obj object containing analysis results
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @export
##' 
prepare_results <- function(obj) {
  UseMethod("prepare_results") 
}