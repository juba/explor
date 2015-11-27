if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("Type", "Contrib.x", "Contrib.y", "Cos2.x", "Cos2.y", 
                           "Level", "Variable", "Coord.x", "Coord.y", "Name", 
                           "P.value", "Class", "Cor", "Cor.x", "Cor.y", "Coord", 
                           "starts_with", "Contrib", "Cos2", "varname", "modname", 
                           "V.test", "eta2"))


##' Interface for analysis results exploration
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive visualisation and exploration of an analysis results.
##'
##' @param obj object containing analysis results
##' @return
##' The function launches a shiny app in the system web browser.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @export
##' @examples
##' \dontrun{
##' 
##' require(FactoMineR)
##' 
##' ## MCA exploration
##' data(hobbies)
##' mca <- MCA(hobbies[1:1000,c(1:8,21:23)], quali.sup = 9:10, 
##'            quanti.sup = 11, ind.sup = 1:100, graph = FALSE)
##' explor(mca)
##' 
##' ## PCA exploration
##' data(decathlon)
##' d <- decathlon[,1:12]
##' pca <- PCA(d, quanti.sup = 11:12, graph = FALSE)
##' explor(pca)
##' }

explor <- function(obj) {
  UseMethod("explor")
}