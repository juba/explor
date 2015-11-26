##' Results preparation for a FactoMineR MCA analysis
##'
##' This function prepares MCA results to be used by \code{explor}. Not to be used directly.
##' 
##' @param obj object containing analysis results
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>

prepare_results <- function(obj) {
  UseMethod("prepare_results") 
}