##' Interface for analysis results exploration
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive visualisation and exploration of an analysis results.
##'
##' @param ... arguments passed to other methods
##' @return
##' The function launches a shiny app in the system web browser.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @export

explor <- function(...) {
  UseMethod("explor") 
}